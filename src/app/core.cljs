(ns app.core
  (:require
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

(defonce repl-state
  (r/atom {:cells [(new-cell)]}))

;; -- Cell operations --

(defn format-result [result]
  (cond
    (nil? result)
    "nil"

    ;; TxReport — avoid dumping full db objects
    (:db-after result)
    (str "TxReport {\n  :tx-data "
         (pr-str (vec (:tx-data result)))
         "\n  :tempids "
         (pr-str (:tempids result))
         "\n}")

    :else
    (let [s (pr-str result)]
      (if (> (count s) 4000)
        (str (subs s 0 4000) "\n;; … (truncated)")
        s))))

(defn- update-cell [cells id f]
  (mapv (fn [c] (if (= (:id c) id) (f c) c)) cells))

(defn eval-cell! [id]
  (let [cell (->> (:cells @repl-state)
                  (filter (fn [c] (= (:id c) id)))
                  first)
        code (:code cell)]
    (try
      (let [result (sci/eval-string* sci-ctx code)]
        (swap! repl-state update :cells update-cell id
               #(assoc % :result (format-result result) :error nil)))
      (catch :default e
        (swap! repl-state update :cells update-cell id
               #(assoc % :result nil :error (.-message e)))))))

(defn format-cell! [id]
  (let [cell (->> (:cells @repl-state)
                  (filter (fn [c] (= (:id c) id)))
                  first)
        code (:code cell)]
    (when (seq code)
      (try
        (let [formatted (zp/zprint-str code {:parse-string? true})]
          (swap! repl-state update :cells update-cell id #(assoc % :code formatted)))
        (catch :default _)))))

(defn run-all! []
  (doseq [{:keys [id]} (:cells @repl-state)]
    (eval-cell! id)))

(defn- ensure-trailing-blank! []
  (swap! repl-state update :cells
         (fn [cells]
           (if (empty? (:code (last cells)))
             cells
             (conj cells (new-cell))))))

(defn load-example-set! [set-id]
  (if (nil? set-id)
    (swap! repl-state assoc :cells [(new-cell)])
    (let [example-set (->> examples/sets
                           (filter (fn [s] (= (:id s) set-id)))
                           first)
          cells (mapv (fn [{:keys [label code]}] (new-cell (str ";; " label "\n" code))) (:examples example-set))]
      (swap! repl-state assoc :cells cells)))
  (ensure-trailing-blank!))

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
     (if (empty? schema)
       [:p
        {:style
         {:font-size "12px"
          :color "#9ca3af"
          :font-family mono}}
        "No schema defined"]
       [:table
        {:style
         {:width "100%"
          :border-collapse "collapse"}}
        [:thead
         [:tr
          (for [label ["Attribute" "Properties"]]
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
              {:padding "3px 8px"
               :color "#2563eb"
               :font-family mono
               :font-size "12px"}}
             (str attr)]
            [:td
             {:style
              {:padding "3px 8px"
               :font-family mono
               :font-size "12px"
               :color "#374151"}}
             (zp/zprint-str (get schema attr))]])]])]))

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

(defn code-editor [{:keys [value on-change on-run]}]
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
                                                      :run (fn [] (on-run) true)}]))
                              EditorView.lineWrapping
                              (EditorView.theme
                                #js {"&.cm-focused" #js {"outline" "none"}
                                     ".cm-content" #js {"padding" "8px"
                                                        "fontFamily" mono
                                                        "fontSize" "13px"
                                                        "lineHeight" "1.6"
                                                        "color" "#1f2937"
                                                        "minHeight" "108px"}
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
                    (swap! repl-state update :cells update-cell id
                           #(assoc % :code new-code))
                    (ensure-trailing-blank!))
       :on-run (fn [] (eval-cell! id))}]
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
    [:div
     {:style {:display "flex" :justify-content "flex-end"}}
     [:button
      {:on-click (fn [_] (format-cell! id))
       :style
       {:background "white"
        :color "#6b7280"
        :border "1px solid #e5e7eb"
        :border-radius "4px"
        :padding "3px 8px"
        :font-size "11px"
        :cursor "pointer"}}
      "Format"]]]
   [:div
    [result-view {:result result :error error}]]])

(defn example-selector []
  [:select
   {:on-change (fn [e]
                 (let [v (.. e -target -value)]
                   (load-example-set! (when (seq v) (keyword v)))))
    :style
    {:font-size "13px"
     :border "1px solid #d1d5db"
     :border-radius "6px"
     :padding "5px 10px"
     :background "white"
     :color "#374151"
     :cursor "pointer"}}
   [:option {:value ""} "Blank"]
   (for [{:keys [id label]} examples/sets]
     [:option {:key (str id) :value (name id)} label])])


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
  (let [{:keys [cells]} @repl-state]
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
     :padding "24px"}}
   [:div
    {:style
     {:max-width "1400px"
      :margin "0 auto"}}
    [:div
     {:style
      {:display "flex"
       :justify-content "space-between"
       :align-items "flex-start"
       :margin-bottom "20px"}}
     [:h1
      {:style
       {:font-size "20px"
        :font-weight "700"
        :color "#111827"}}
      "DataScript Playground"]
     [:div
      {:style
       {:display "flex"
        :align-items "center"
        :gap "8px"}}
      [:span
       {:style {:font-size "13px" :color "#6b7280"}}
       "Load example:"]
      [example-selector]]]
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

(defn init []
  (reset! root (rdom/create-root (.getElementById js/document "app")))
  (re-render))