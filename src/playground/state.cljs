(ns playground.state
  (:require
   [clojure.string :as str]
   [cljs.reader :as reader]
   [datascript.core :as ds]
   [reagent.core :as r]
   [sci.core :as sci]
   [zprint.core :as zp]
   [playground.examples :as examples]))

(defonce conn (r/atom (ds/create-conn)))

(defonce db-state (r/atom (ds/db @conn)))

(defonce db-history (r/atom []))
(defonce db-history-idx (r/atom 0))

(defn wire-listener! [ds-conn]
  (ds/listen! ds-conn ::watcher
    (fn [{:keys [db-after]}]
      (reset! db-state db-after)
      (let [new-hist (swap! db-history conj db-after)]
        (reset! db-history-idx (dec (count new-hist)))))))

(wire-listener! @conn)
(reset! db-history [(ds/db @conn)])

(add-watch conn ::conn-swap
  (fn [_ _ old-ds-conn new-ds-conn]
    (ds/unlisten! old-ds-conn ::watcher)
    (wire-listener! new-ds-conn)
    (reset! db-state (ds/db new-ds-conn))))

(defn update-schema! [conn schema-updates]
  (let [current-db (ds/db @conn)
        new-schema (merge (ds/schema current-db) schema-updates)
        new-db (ds/init-db (ds/datoms current-db :eavt) new-schema)]
    (reset! conn (ds/conn-from-db new-db))
    (swap! db-history
           (fn [h]
             (if (empty? h)
               [new-db]
               (assoc h (dec (count h)) new-db))))))

(defn db->snapshot
  ([] (db->snapshot (ds/db @conn)))
  ([db]
   {:schema (ds/schema db)
    :eavs (->> (ds/datoms db :eavt)
               (remove (fn [d] (= (:a d) :db/txInstant)))
               (mapv (fn [d] [(:e d) (:a d) (:v d)])))}))

(defn snapshot->conn [{:keys [schema eavs]}]
  (let [c (ds/create-conn schema)]
    (when (seq eavs)
      (ds/transact! c (mapv (fn [[e a v]] (assoc {a v} :db/id e)) eavs)))
    c))

(defn snapshot->db [snapshot]
  (ds/db (snapshot->conn snapshot)))

;; -- SCI context --
;; Pre-requires datascript.core as d, binds conn atom directly.

(defonce sci-ctx
  (let [ctx (sci/init
              {:namespaces
               {'datascript.core
                {'q ds/q
                 'pull ds/pull
                 'pull-many ds/pull-many
                 'entity ds/entity
                 'datoms ds/datoms
                 'touch ds/touch
                 'transact! ds/transact!
                 'db ds/db
                 'create-conn ds/create-conn
                 'empty-db ds/empty-db
                 'conn-from-db ds/conn-from-db
                 'db-with ds/db-with
                 'init-db ds/init-db
                 'schema ds/schema}}
               :bindings
               {'conn conn
                'update-schema! update-schema!}})]
    (sci/eval-string* ctx "(require '[datascript.core :as d])")
    ctx))

;; -- Cell state --

(defonce next-id (atom 0))

(defn new-id! [] (swap! next-id inc))

(defn new-cell
  ([] (new-cell ""))
  ([code] {:id (new-id!) :code code :result nil :error nil}))

(defonce next-tab-id (atom 1))

(defn new-tab-id! [] (swap! next-tab-id inc))

(defonce tabs-state
  (r/atom {:active-tab-id 1
           :tabs [{:id 1 :label "Tab 1" :cells [(new-cell)] :db-snapshot nil}]}))

;; -- Tab helpers --

(defn active-tab []
  (let [{:keys [tabs active-tab-id]} @tabs-state]
    (first (filter (fn [t] (= (:id t) active-tab-id)) tabs))))

(defn update-active-tab! [f]
  (swap! tabs-state
         (fn [{:keys [active-tab-id] :as state}]
           (update state :tabs
                   (fn [tabs]
                     (mapv (fn [tab]
                             (if (= (:id tab) active-tab-id)
                               (f tab)
                               tab))
                           tabs))))))

;; -- Cell operations --

(defn format-result [result]
  (let [s (pr-str result)]
    (cond
      (record? result)
      s
      (or (map? result) (sequential? result) (set? result))
      (try
        (zp/zprint-str result {:width 60
                          :map {:comma? false}
                          :style :respect-nl})
        (catch :default _ s))
      :else s)))

(defn update-cell [cells id f]
  (mapv (fn [c] (if (= (:id c) id) (f c) c)) cells))

(defn eval-cell! [id]
  (let [cell (->> (:cells (active-tab))
                  (filter (fn [c] (= (:id c) id)))
                  first)
        code (:code cell)]
    (try
      (let [result (sci/eval-string* sci-ctx code)]
        (update-active-tab!
          #(update % :cells update-cell id
                   (fn [c] (assoc c :result (format-result result) :error nil)))))
      (catch :default e
        (update-active-tab!
          #(update % :cells update-cell id
                   (fn [c] (assoc c :result nil :error (.-message e)))))))))

(defn format-code [code]
  (try
    (zp/zprint-str code {:parse-string-all? true
                         :width 60
                         :map {:comma? false}
                         :style :respect-nl})
    (catch :default _ code)))

(defn format-cell! [id]
  (let [cell (->> (:cells (active-tab))
                  (filter (fn [c] (= (:id c) id)))
                  first)
        code (:code cell)]
    (when (seq code)
      (update-active-tab!
        #(update % :cells update-cell id (fn [c] (assoc c :code (format-code code))))))))

(defn run-all! []
  (doseq [{:keys [id]} (:cells (active-tab))]
    (eval-cell! id)))

(defn download-cells! []
  (let [codes (->> (:cells (active-tab))
                   (map :code)
                   (filter seq))
        content (str "(require '[datascript.core :as d])\n\n"
                     "(def conn (atom (d/create-conn)))\n\n"
                     "(defn update-schema! [conn schema-updates]\n"
                     "  (let [current-db (d/db @conn)\n"
                     "        new-schema (merge (d/schema current-db) schema-updates)]\n"
                     "    (reset! conn\n"
                     "            (d/conn-from-db\n"
                     "             (d/init-db (d/datoms current-db :eavt) new-schema)))))\n\n"
                     (str/join "\n\n" codes))
        blob (js/Blob. #js [content] #js {:type "text/plain"})
        url (.createObjectURL js/URL blob)
        a (.createElement js/document "a")]
    (set! (.-href a) url)
    (set! (.-download a) "notebook.clj")
    (.appendChild (.-body js/document) a)
    (.click a)
    (.removeChild (.-body js/document) a)
    (.revokeObjectURL js/URL url)))

(defn ensure-trailing-blank! []
  (update-active-tab!
    #(update % :cells
             (fn [cells]
               (if (empty? (:code (last cells)))
                 cells
                 (conj cells (new-cell)))))))

(defn delete-cell! [id]
  (update-active-tab!
    #(update % :cells (fn [cells] (vec (remove (fn [c] (= (:id c) id)) cells)))))
  (ensure-trailing-blank!))

;; -- Storage --

(def storage-key "datascript-playground-tabs")

(defn storable-state []
  (let [state @tabs-state
        active-id (:active-tab-id state)]
    (update state :tabs
            (fn [tabs]
              (mapv (fn [tab]
                      (-> tab
                          (cond-> (= (:id tab) active-id)
                            (assoc :db-history-snapshots (mapv db->snapshot @db-history)
                                   :db-history-idx @db-history-idx))
                          (dissoc :db-snapshot)
                          (update :cells (fn [cells]
                                           (mapv #(select-keys % [:id :code]) cells)))))
                    tabs)))))

(defn save-to-storage! []
  (.setItem js/localStorage storage-key (pr-str (storable-state))))

(defn load-from-storage []
  (when-let [raw (.getItem js/localStorage storage-key)]
    (try
      (reader/read-string raw)
      (catch :default _ nil))))

;; -- Tab management --

(defn cells-for-set [set-id]
  (let [example-set (->> examples/sets
                         (filter (fn [s] (= (:id s) set-id)))
                         first)]
    (mapv (fn [{:keys [label code]}]
            (let [label-comment (->> (str/split-lines label)
                                     (map (fn [l] (str ";; " l)))
                                     (str/join "\n"))]
              (new-cell (format-code (str label-comment "\n" code)))))
          (:examples example-set))))

(defn switch-tab! [tab-id]
  (let [history-snaps (mapv db->snapshot @db-history)
        history-idx @db-history-idx]
    (swap! tabs-state
           (fn [{:keys [active-tab-id] :as state}]
             (-> state
                 (update :tabs (fn [tabs]
                                 (mapv (fn [tab]
                                         (if (= (:id tab) active-tab-id)
                                           (assoc tab
                                                  :db-history-snapshots history-snaps
                                                  :db-history-idx history-idx)
                                           tab))
                                       tabs)))
                 (assoc :active-tab-id tab-id))))
    (let [new-tab (first (filter (fn [t] (= (:id t) tab-id)) (:tabs @tabs-state)))
          snaps (or (:db-history-snapshots new-tab)
                    (when-let [s (:db-snapshot new-tab)] [s])
                    [])
          idx (or (:db-history-idx new-tab) (max 0 (dec (count snaps))))]
      (reset! db-history (mapv snapshot->db snaps))
      (reset! db-history-idx idx)
      (reset! conn (if (seq snaps)
                     (snapshot->conn (last snaps))
                     (ds/create-conn))))
    (ensure-trailing-blank!)))

(defn add-tab! [set-id]
  (let [history-snaps (mapv db->snapshot @db-history)
        history-idx @db-history-idx
        tab-id (new-tab-id!)
        [label cells] (if (nil? set-id)
                        [(str "Tab " tab-id) [(new-cell)]]
                        (let [example-set (->> examples/sets
                                               (filter (fn [s] (= (:id s) set-id)))
                                               first)]
                          [(:label example-set) (cells-for-set set-id)]))]
    (swap! tabs-state
           (fn [{:keys [active-tab-id] :as state}]
             (-> state
                 (update :tabs (fn [tabs]
                                 (mapv (fn [tab]
                                         (if (= (:id tab) active-tab-id)
                                           (assoc tab
                                                  :db-history-snapshots history-snaps
                                                  :db-history-idx history-idx)
                                           tab))
                                       tabs)))
                 (update :tabs conj {:id tab-id :label label :cells cells})
                 (assoc :active-tab-id tab-id))))
    (reset! conn (ds/create-conn))
    (reset! db-history [(ds/db @conn)])
    (reset! db-history-idx 0)
    (ensure-trailing-blank!)))

(defn remove-tab! [tab-id]
  (let [{:keys [tabs active-tab-id]} @tabs-state]
    (when (> (count tabs) 1)
      (let [idx (first (keep-indexed (fn [i t] (when (= (:id t) tab-id) i)) tabs))
            remaining (filterv (fn [t] (not= (:id t) tab-id)) tabs)
            new-active-id (if (= active-tab-id tab-id)
                            (:id (or (get tabs (inc idx)) (get tabs (dec idx))))
                            active-tab-id)]
        (swap! tabs-state assoc :tabs remaining :active-tab-id new-active-id)
        (when (= active-tab-id tab-id)
          (let [new-active (first (filter (fn [t] (= (:id t) new-active-id)) remaining))
                snaps (or (:db-history-snapshots new-active)
                          (when-let [s (:db-snapshot new-active)] [s])
                          [])
                restore-idx (or (:db-history-idx new-active) (max 0 (dec (count snaps))))]
            (reset! db-history (mapv snapshot->db snaps))
            (reset! db-history-idx restore-idx)
            (reset! conn (if (seq snaps)
                           (snapshot->conn (last snaps))
                           (ds/create-conn)))))))))

(defn rename-tab! [tab-id new-label]
  (swap! tabs-state update :tabs
         (fn [tabs]
           (mapv (fn [tab]
                   (if (= (:id tab) tab-id)
                     (assoc tab :label new-label)
                     tab))
                 tabs))))

(add-watch tabs-state ::storage (fn [_ _ _ _] (save-to-storage!)))
(add-watch db-state ::storage (fn [_ _ _ _] (save-to-storage!)))

(defn load-saved-state! []
  (if-let [saved (load-from-storage)]
    (do
      (let [all-cell-ids (->> (:tabs saved) (mapcat :cells) (map :id) (filter int?))]
        (when (seq all-cell-ids)
          (reset! next-id (apply max all-cell-ids))))
      (let [all-tab-ids (->> (:tabs saved) (map :id) (filter int?))]
        (when (seq all-tab-ids)
          (reset! next-tab-id (apply max all-tab-ids))))
      (let [active-id (:active-tab-id saved)
            active (first (filter (fn [t] (= (:id t) active-id)) (:tabs saved)))
            snaps (or (:db-history-snapshots active)
                      (when-let [s (:db-snapshot active)] [s])
                      [])
            idx (or (:db-history-idx active) (max 0 (dec (count snaps))))]
        (when (seq snaps)
          (reset! db-history (mapv snapshot->db snaps))
          (reset! db-history-idx idx)
          (reset! conn (snapshot->conn (last snaps)))))
      (reset! tabs-state
              (update saved :tabs
                      (fn [tabs]
                        (mapv (fn [tab]
                                (update tab :cells
                                        (fn [cells]
                                          (mapv (fn [{:keys [id code]}]
                                                  {:id id :code code :result nil :error nil})
                                                cells))))
                              tabs)))))
    (let [default-label :social
          default (->> examples/sets
                       (filter (fn [s] (= (:id s) default-label)))
                       first)]
      (swap! tabs-state assoc :tabs [{:id 1
                                      :label (:label default)
                                      :cells (cells-for-set :friends)}]))))
