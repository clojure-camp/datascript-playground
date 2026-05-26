(ns app.core
  (:require
   [cljs.reader :as reader]
   [clojure.string :as str]
   [app.examples :as examples]
   [datascript.core :as ds]
   [reagent.core :as r]
   [reagent.dom.client :as rdom]
   [sci.core :as sci]
   [zprint.core :as zp]
   ["@codemirror/commands" :refer [defaultKeymap history historyKeymap]]
   ["@codemirror/language" :refer [StreamLanguage defaultHighlightStyle syntaxHighlighting]]
   ["@codemirror/legacy-modes/mode/clojure" :as cm-clojure]
   ["@codemirror/state" :refer [EditorState Prec]]
   ["@codemirror/view" :refer [EditorView keymap]]
   ["@jurjanpaul/codemirror6-parinfer" :refer [parinferExtension]]))

;; -- Database --

(defonce conn (r/atom (ds/create-conn)))

(defonce db-state (r/atom (ds/db @conn)))

(defn- wire-listener! [ds-conn]
  (ds/listen! ds-conn ::watcher
    (fn [{:keys [db-after]}]
      (reset! db-state db-after))))

(wire-listener! @conn)

(add-watch conn ::conn-swap
  (fn [_ _ old-ds-conn new-ds-conn]
    (ds/unlisten! old-ds-conn ::watcher)
    (wire-listener! new-ds-conn)
    (reset! db-state (ds/db new-ds-conn))))

(defn update-schema! [conn schema-updates]
  (let [current-db (ds/db @conn)
        new-schema (merge (ds/schema current-db) schema-updates)]
    (reset! conn
            (ds/conn-from-db
             (ds/init-db (ds/datoms current-db :eavt) new-schema)))))

(defn- db->snapshot []
  (let [db (ds/db @conn)]
    {:schema (ds/schema db)
     :eavs (->> (ds/datoms db :eavt)
                (remove (fn [d] (= (:a d) :db/txInstant)))
                (mapv (fn [d] [(:e d) (:a d) (:v d)])))}))


(defn- snapshot->conn [{:keys [schema eavs]}]
  (let [c (ds/create-conn schema)]
    (when (seq eavs)
      (ds/transact! c (mapv (fn [[e a v]] (assoc {a v} :db/id e)) eavs)))
    c))

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

(defn- new-id! [] (swap! next-id inc))

(defn- new-cell
  ([] (new-cell ""))
  ([code] {:id (new-id!) :code code :result nil :error nil}))

(defonce next-tab-id (atom 1))

(defn- new-tab-id! [] (swap! next-tab-id inc))

(defonce tabs-state
  (r/atom {:active-tab-id 1
           :tabs [{:id 1 :label "Tab 1" :cells [(new-cell)] :db-snapshot nil}]}))

;; -- Tab helpers --

(defn- active-tab []
  (let [{:keys [tabs active-tab-id]} @tabs-state]
    (first (filter (fn [t] (= (:id t) active-tab-id)) tabs))))

(defn- update-active-tab! [f]
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
      (or (map? result) (sequential? result) (set? result))
      (try
        (zp/zprint-str s {:width 60
                          :map {:comma? false}
                          :style :respect-nl})
        (catch :default _ s))
      :else s)))

(defn- update-cell [cells id f]
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

(defn- format-code [code]
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

(defn- ensure-trailing-blank! []
  (update-active-tab!
    #(update % :cells
             (fn [cells]
               (if (empty? (:code (last cells)))
                 cells
                 (conj cells (new-cell)))))))

;; -- Storage --

(def ^:private storage-key "datascript-playground-tabs")

(defn- storable-state []
  (let [state @tabs-state
        active-id (:active-tab-id state)]
    (update state :tabs
            (fn [tabs]
              (mapv (fn [tab]
                      (-> tab
                          (assoc :db-snapshot
                                 (if (= (:id tab) active-id)
                                   (db->snapshot)
                                   (:db-snapshot tab)))
                          (update :cells (fn [cells]
                                           (mapv #(select-keys % [:id :code]) cells)))))
                    tabs)))))

(defn- save-to-storage! []
  (.setItem js/localStorage storage-key (pr-str (storable-state))))

(defn- load-from-storage []
  (when-let [raw (.getItem js/localStorage storage-key)]
    (try
      (reader/read-string raw)
      (catch :default _ nil))))

;; -- Tab management --

(defn- cells-for-set [set-id]
  (let [example-set (->> examples/sets
                         (filter (fn [s] (= (:id s) set-id)))
                         first)]
    (mapv (fn [{:keys [label code]}]
            (new-cell (format-code (str ";; " label "\n" code))))
          (:examples example-set))))

(defn switch-tab! [tab-id]
  (let [snapshot (db->snapshot)]
    (swap! tabs-state
           (fn [{:keys [active-tab-id] :as state}]
             (-> state
                 (update :tabs (fn [tabs]
                                 (mapv (fn [tab]
                                         (if (= (:id tab) active-tab-id)
                                           (assoc tab :db-snapshot snapshot)
                                           tab))
                                       tabs)))
                 (assoc :active-tab-id tab-id))))
    (let [new-tab (first (filter (fn [t] (= (:id t) tab-id)) (:tabs @tabs-state)))]
      (reset! conn (if-let [snap (:db-snapshot new-tab)]
                     (snapshot->conn snap)
                     (ds/create-conn))))
    (ensure-trailing-blank!)))

(defn add-tab! [set-id]
  (let [snapshot (db->snapshot)
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
                                           (assoc tab :db-snapshot snapshot)
                                           tab))
                                       tabs)))
                 (update :tabs conj {:id tab-id :label label :cells cells :db-snapshot nil})
                 (assoc :active-tab-id tab-id))))
    (reset! conn (ds/create-conn))
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
          (let [new-active (first (filter (fn [t] (= (:id t) new-active-id)) remaining))]
            (reset! conn (if-let [snap (:db-snapshot new-active)]
                           (snapshot->conn snap)
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

;; -- Styles --

(def mono "ui-monospace, 'Cascadia Code', 'Fira Code', monospace")

;; -- Components --

(defn schema-panel []
  (let [schema (ds/schema @db-state)
        attrs (sort (keys schema))]
    [:div
     {:style
      {:background "white"
       :border-radius "8px"
       :border "1px solid #e5e7eb"
       :padding "16px"}}
     [:h2
      {:style
       {:font-size "11px"
        :font-weight "600"
        :color "#9ca3af"
        :letter-spacing "0.08em"
        :text-transform "uppercase"
        :margin-bottom "12px"}}
      "Schema"]
     (if (empty? attrs)
       [:p
        {:style {:font-size "12px" :color "#9ca3af" :font-style "italic"}}
        "No schema defined yet"]
       [:table
        {:style {:width "100%" :border-collapse "collapse"}}
        [:thead
         [:tr
          (for [label ["Attribute" "Type" "Cardinality"]]
            [:th
             {:key label
              :style
              {:text-align "left"
               :padding "4px 8px"
               :color "#9ca3af"
               :font-size "11px"
               :font-family mono
               :font-weight "500"
               :border-bottom "2px solid #f3f4f6"}}
             label])]]
        [:tbody
         (for [attr attrs]
           [:tr
            {:key (str attr)}
            [:td
             {:style
              {:padding "4px 8px"
               :color "#2563eb"
               :font-family mono
               :font-size "12px"}}
             (str attr)]
            [:td
             {:style
              {:padding "4px 8px"
               :font-family mono
               :font-size "12px"
               :color "#374151"}}
             (-> schema (get attr) :db/valueType str (str/replace #"^:db.type/" ""))]
            [:td
             {:style
              {:padding "4px 8px"
               :font-family mono
               :font-size "12px"
               :color "#374151"}}
             (-> schema (get attr) :db/cardinality str (str/replace #"^:db.cardinality/" ""))]])]])]))

(defn datoms-panel []
  (let [db @db-state
        datoms (ds/datoms db :eavt)]
    [:div
     {:style
      {:background "white"
       :border-radius "8px"
       :border "1px solid #e5e7eb"
       :padding "16px"
       :overflow "auto"
       :max-height "calc(100vh - 120px)"}}
     [:h2
      {:style
       {:font-size "11px"
        :font-weight "600"
        :color "#9ca3af"
        :letter-spacing "0.08em"
        :text-transform "uppercase"
        :margin-bottom "12px"}}
      (str "Datoms (" (count datoms) ")")]
     [:table
      {:style
       {:width "100%"
        :border-collapse "collapse"}}
      [:thead
       [:tr
        (for [label ["E" "A" "V" "Tx"]]
          [:th
           {:key label
            :style
            {:text-align "left"
             :padding "4px 8px"
             :color "#9ca3af"
             :font-size "11px"
             :font-family mono
             :font-weight "500"
             :border-bottom "2px solid #f3f4f6"}}
           label])]]
      [:tbody
       (map-indexed
         (fn [i datom]
           [:tr
            {:key i}
            [:td
             {:style
              {:padding "3px 8px"
               :color "#9ca3af"
               :font-family mono
               :font-size "12px"}}
             (:e datom)]
            [:td
             {:style
              {:padding "3px 8px"
               :color "#2563eb"
               :font-family mono
               :font-size "12px"}}
             (str (:a datom))]
            [:td
             {:style
              {:padding "3px 8px"
               :font-family mono
               :font-size "12px"
               :max-width "220px"
               :overflow "hidden"
               :text-overflow "ellipsis"
               :white-space "nowrap"}}
             (pr-str (:v datom))]
            [:td
             {:style
              {:padding "3px 8px"
               :color "#d1d5db"
               :font-family mono
               :font-size "12px"}}
             (:tx datom)]])
         datoms)]]]))

(defn result-view [{:keys [result error]}]
  (cond
    error
    [:pre
     {:style
      {:background "#fef2f2"
       :border "1px solid #fecaca"
       :border-radius "6px"
       :padding "10px"
       :font-size "12px"
       :font-family mono
       :color "#dc2626"
       :overflow "auto"
       :white-space "pre-wrap"
       :margin 0}}
     error]

    result
    [:pre
     {:style
      {:background "#f8fafc"
       :border "1px solid #e2e8f0"
       :border-radius "6px"
       :padding "10px"
       :font-size "12px"
       :font-family mono
       :color "#1f2937"
       :overflow "auto"
       :white-space "pre-wrap"
       :margin 0}}
     result]

    :else nil))

(defn code-editor [{:keys [value on-change on-run on-format]}]
  (let [!view (atom nil)
        !container (atom nil)]
    (r/create-class
      {:display-name "code-editor"

       :component-did-mount
       (fn [_]
         (reset! !view
           (EditorView.
             #js {:state
                  (EditorState.create
                    #js {:doc value
                         :extensions
                         #js [(history)
                              (parinferExtension)
                              (syntaxHighlighting defaultHighlightStyle)
                              (StreamLanguage.define (.-clojure cm-clojure))
                              (.of keymap (.concat defaultKeymap historyKeymap))
                              (.highest Prec
                                (.of keymap #js [#js {:key "Mod-Enter"
                                                      :run (fn [] (on-run) true)}
                                                #js {:key "Tab"
                                                     :run (fn [] (on-format) true)}]))
                              EditorView.lineWrapping
                              (EditorView.theme
                                #js {"&.cm-focused" #js {"outline" "none"}
                                     ".cm-content" #js {"padding" "8px"
                                                        "fontFamily" mono
                                                        "fontSize" "13px"
                                                        "lineHeight" "1.6"
                                                        "color" "#1f2937"
                                                        "boxSizing" "content-box"
                                                        "minHeight" "108px"
                                                        "minWidth" "60ch"}
                                     ".cm-line" #js {"padding" "0"}})
                              (.of EditorView.updateListener
                                (fn [^js upd]
                                  (when (.-docChanged upd)
                                    (on-change (.toString (.. upd -state -doc))))))]})
                  :parent @!container})))

       :component-did-update
       (fn [this _]
         (let [new-val (:value (r/props this))
               view @!view
               cur-val (.toString (.. view -state -doc))]
           (when (not= new-val cur-val)
             (.dispatch view
               #js {:changes #js {:from 0
                                   :to (.. view -state -doc -length)
                                   :insert new-val}}))))

       :component-will-unmount
       (fn [_]
         (some-> @!view .destroy))

       :reagent-render
       (fn [_]
         [:div
          {:ref (fn [el] (reset! !container el))
           :style
           {:border "1px solid #d1d5db"
            :border-radius "6px"
            :overflow "hidden"
            :flex 1}}])})))

(defn cell-view [{:keys [id code result error]}]
  [:div
   {:style
    {:display "grid"
     :grid-template-columns "1fr 1fr"
     :gap "16px"
     :padding "12px 0"
     :border-bottom "1px solid #f3f4f6"}}
   [:div
    {:style
     {:display "flex"
      :flex-direction "column"
      :gap "6px"}}
    [:div
     {:style
      {:display "flex"
       :gap "6px"
       :align-items "flex-start"}}
     [code-editor
      {:value code
       :on-change (fn [new-code]
                    (update-active-tab!
                      #(update % :cells update-cell id
                               (fn [c] (assoc c :code new-code))))
                    (ensure-trailing-blank!))
       :on-run (fn [] (eval-cell! id))
       :on-format (fn [] (format-cell! id))}]
     [:button
      {:on-mouse-down (fn [e] (.preventDefault e))
       :on-click (fn [_] (eval-cell! id))
       :title "Run (⌘+Enter)"
       :style
       {:background "#4f46e5"
        :color "white"
        :border "none"
        :border-radius "4px"
        :padding "5px 9px"
        :cursor "pointer"
        :font-size "13px"
        :line-height "1"
        :flex-shrink 0
        :align-self "flex-start"}}
      "▶"]]
]
   [:div
    [result-view {:result result :error error}]]])

(defn tabs-bar []
  (let [template (r/atom "")]
    (fn []
      (let [{:keys [tabs active-tab-id]} @tabs-state]
        [:div
         {:style
          {:display "flex"
           :align-items "center"
           :gap "6px"
           :flex-wrap "wrap"}}
         (for [{:keys [id label]} tabs]
           [:div
            {:key (str id)
             :on-click (fn [_] (when (not= id active-tab-id) (switch-tab! id)))
             :style
             {:display "flex"
              :align-items "center"
              :gap "4px"
              :background (if (= id active-tab-id) "white" "#f3f4f6")
              :border (str "1px solid " (if (= id active-tab-id) "#6366f1" "#d1d5db"))
              :border-radius "6px"
              :padding "5px 10px"
              :font-size "13px"
              :font-weight (if (= id active-tab-id) "600" "400")
              :color (if (= id active-tab-id) "#4f46e5" "#374151")
              :cursor (if (= id active-tab-id) "default" "pointer")
              :user-select "none"}}
            [:span
             {:on-double-click (fn [e]
                                 (.stopPropagation e)
                                 (when-let [new-label (js/prompt "Rename tab:" label)]
                                   (when (seq new-label)
                                     (rename-tab! id new-label))))}
             label]
            (when (> (count tabs) 1)
              [:span
               {:on-click (fn [e]
                            (.stopPropagation e)
                            (remove-tab! id))
                :style
                {:color "#9ca3af"
                 :font-size "15px"
                 :line-height "1"
                 :cursor "pointer"
                 :padding "0 1px"
                 :margin-left "4px"}}
               "×"])])
         [:div
          {:style
           {:margin-left "auto"
            :display "flex"
            :align-items "center"
            :gap "6px"}}
          [:select
           {:value @template
            :on-change (fn [e]
                         (let [v (.. e -target -value)]
                           (when (seq v)
                             (add-tab! (when (not= v "blank") (keyword v)))
                             (reset! template ""))))
            :style
            {:font-size "13px"
             :border "1px solid #d1d5db"
             :border-radius "6px"
             :padding "5px 10px"
             :background "white"
             :color "#374151"
             :cursor "pointer"}}
           [:option {:value ""} "Add Tab..."]
           [:option {:value "blank"} "Blank"]
           (for [{:keys [id label]} examples/sets]
             [:option {:key (str id) :value (name id)} label])]]]))))

(defn code-badge [s]
  [:code
   {:style
    {:font-family mono
     :background "#f3f4f6"
     :padding "1px 5px"
     :border-radius "3px"
     :font-size "12px"
     :color "#374151"}}
   s])

(defn notebook-panel []
  (let [{:keys [cells]} (active-tab)]
    [:div
     {:style
      {:background "white"
       :border-radius "8px"
       :border "1px solid #e5e7eb"
       :padding "16px"}}
     [:div
      {:style
       {:display "flex"
        :justify-content "space-between"
        :align-items "center"
        :margin-bottom "8px"}}
      [:div
       {:style {:display "flex" :align-items "center" :gap "12px"}}
       [:h2
        {:style
         {:font-size "11px"
          :font-weight "600"
          :color "#9ca3af"
          :letter-spacing "0.08em"
          :text-transform "uppercase"}}
        "Notebook"]
       [:p
        {:style
         {:font-size "12px"
          :color "#9ca3af"
          :display "flex"
          :align-items "center"
          :gap "4px"
          :flex-wrap "wrap"
          :margin 0}}
        "Bindings: "
        [code-badge "conn"] " · "
        [code-badge "d/q"] " · "
        [code-badge "d/transact!"] " · "
        [code-badge "d/pull"] " · "
        [code-badge "d/datoms"] " · "
        [code-badge "d/entity"] " · "
        [code-badge "d/touch"] " · "
        [code-badge "update-schema!"]]]
      [:div
       {:style {:display "flex" :gap "8px"}}
       [:button
        {:on-click (fn [_] (download-cells!))
         :style
         {:background "white"
          :color "#374151"
          :border "1px solid #d1d5db"
          :border-radius "6px"
          :padding "5px 14px"
          :font-size "12px"
          :font-weight "500"
          :cursor "pointer"}}
        "Download"]
       [:button
        {:on-click (fn [_] (run-all!))
         :style
         {:background "#4f46e5"
          :color "white"
          :border "none"
          :border-radius "6px"
          :padding "5px 14px"
          :font-size "12px"
          :font-weight "500"
          :cursor "pointer"}}
        "Run All"]]]
     [:div
      {:style
       {:display "grid"
        :grid-template-columns "1fr 1fr"
        :gap "16px"
        :padding-bottom "6px"
        :border-bottom "2px solid #f3f4f6"}}
      [:span
       {:style
        {:font-size "11px"
         :font-weight "500"
         :color "#9ca3af"
         :font-family mono}}
       "Code"]
      [:span
       {:style
        {:font-size "11px"
         :font-weight "500"
         :color "#9ca3af"
         :font-family mono}}
       "Result"]]
     (for [{:keys [id] :as cell} cells]
       ^{:key id} [cell-view cell])]))


(defn app []
  [:div
   {:style
    {:min-height "100vh"
     :background "#f9fafb"
     :padding "12px"}}
   [:div
    {:style
     {:margin "0 auto"}}
    [:div
     {:style
      {:display "flex"
       :align-items "center"
       :gap "12px"
       :margin-bottom "12px"
       :flex-wrap "wrap"}}
     [:h1
      {:style
       {:font-size "20px"
        :font-weight "700"
        :color "#111827"
        :margin 0}}
      "DataScript Playground"]
     [:div {:style {:flex 1}}]
     [tabs-bar]]
    [:div
     {:style
      {:display "grid"
       :grid-template-columns "25em 1fr"
       :gap "20px"
       :align-items "start"}}
     [:div
      {:style
       {:display "flex"
        :flex-direction "column"
        :gap "20px"
        :position "sticky"
        :top "24px"
        :align-self "start"}}
      [schema-panel]
      [datoms-panel]]
     [notebook-panel]]]])

(defonce root (atom nil))

(defn ^:dev/after-load re-render []
  (rdom/render @root [app]))

(defn- load-saved-state! []
  (when-let [saved (load-from-storage)]
    (let [all-cell-ids (->> (:tabs saved) (mapcat :cells) (map :id) (filter int?))]
      (when (seq all-cell-ids)
        (reset! next-id (apply max all-cell-ids))))
    (let [all-tab-ids (->> (:tabs saved) (map :id) (filter int?))]
      (when (seq all-tab-ids)
        (reset! next-tab-id (apply max all-tab-ids))))
    (let [active-id (:active-tab-id saved)
          active (first (filter (fn [t] (= (:id t) active-id)) (:tabs saved)))]
      (when active
        (reset! conn (if-let [snap (:db-snapshot active)]
                       (snapshot->conn snap)
                       (ds/create-conn)))))
    (reset! tabs-state
            (update saved :tabs
                    (fn [tabs]
                      (mapv (fn [tab]
                              (update tab :cells
                                      (fn [cells]
                                        (mapv (fn [{:keys [id code]}]
                                                {:id id :code code :result nil :error nil})
                                              cells))))
                            tabs))))))

(defn init []
  (load-saved-state!)
  (reset! root (rdom/create-root (.getElementById js/document "app")))
  (re-render))
