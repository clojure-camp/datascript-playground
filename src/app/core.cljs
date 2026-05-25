(ns app.core
  (:require
   [datascript.core :as ds]
   [reagent.core :as r]
   [reagent.dom.client :as rdom]
   [sci.core :as sci]))

;; -- Database --

(def schema
  {:person/friend
   {:db/cardinality :db.cardinality/many
    :db/valueType :db.type/ref}})

(defonce conn (ds/create-conn schema))

(defonce _seed
  (ds/transact! conn
    [{:db/id -1
      :person/name "Alice"
      :person/age 30}
     {:db/id -2
      :person/name "Bob"
      :person/age 25}
     {:db/id -3
      :person/name "Charlie"
      :person/age 35}
     {:db/id -1
      :person/friend -2}
     {:db/id -1
      :person/friend -3}]))

(defonce db-state (r/atom @conn))

(ds/listen! conn ::watcher
  (fn [{:keys [db-after]}]
    (reset! db-state db-after)))

;; -- SCI context --
;; Pre-requires datascript.core as d, binds conn directly.

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
                 'schema ds/schema}}
               :bindings
               {'conn conn}})]
    (sci/eval-string* ctx "(require '[datascript.core :as d])")
    ctx))

;; -- REPL state --

(defonce repl-state
  (r/atom
    {:code "(d/q '[:find ?e ?name ?age\n       :where\n       [?e :person/name ?name]\n       [?e :person/age ?age]]\n     @conn)"
     :result nil
     :error nil}))

(def examples
  [{:label "Names & ages"
    :code "(d/q '[:find ?e ?name ?age\n       :where\n       [?e :person/name ?name]\n       [?e :person/age ?age]]\n     @conn)"}
   {:label "Pull *"
    :code "(d/pull @conn '[*] 1)"}
   {:label "All datoms"
    :code "(seq (d/datoms @conn :eavt))"}
   {:label "Entity lookup"
    :code "(d/entity @conn 1)"}
   {:label "Friends of Alice"
    :code "(d/q '[:find ?name\n       :where\n       [1 :person/friend ?f]\n       [?f :person/name ?name]]\n     @conn)"}
   {:label "Add person"
    :code "(d/transact! conn\n  [{:person/name \"Dave\"\n    :person/age 28}])"}
   {:label "Retract attr"
    :code "(d/transact! conn\n  [[:db/retract 2 :person/age 25]])"}])

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

(defn eval-code! []
  (let [code (:code @repl-state)]
    (try
      (let [result (sci/eval-string* sci-ctx code)]
        (swap! repl-state assoc
               :result (format-result result)
               :error nil))
      (catch :default e
        (swap! repl-state assoc
               :result nil
               :error (.-message e))))))

;; -- Styles --

(def mono "ui-monospace, 'Cascadia Code', 'Fira Code', monospace")

;; -- Components --

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
  [:div
   [:div
    {:style
     {:font-size "11px"
      :font-weight "600"
      :color "#9ca3af"
      :letter-spacing "0.08em"
      :text-transform "uppercase"
      :margin-bottom "6px"}}
    "Result"]
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

     :else
     [:div
      {:style
       {:font-size "13px"
        :color "#9ca3af"
        :font-style "italic"
        :padding "8px 0"}}
      "Hit Evaluate to see results"])])

(defn repl-panel []
  (let [{:keys [code result error]} @repl-state]
    [:div
     {:style
      {:background "white"
       :border-radius "8px"
       :border "1px solid #e5e7eb"
       :padding "16px"
       :display "flex"
       :flex-direction "column"
       :gap "12px"}}
     [:h2
      {:style
       {:font-size "11px"
        :font-weight "600"
        :color "#9ca3af"
        :letter-spacing "0.08em"
        :text-transform "uppercase"}}
      "REPL"]
     [:div
      {:style
       {:display "flex"
        :flex-wrap "wrap"
        :gap "6px"}}
      (for [{:keys [label code]} examples]
        [:button
         {:key label
          :on-click (fn [_] (swap! repl-state assoc :code code :result nil :error nil))
          :style
          {:background "#f9fafb"
           :border "1px solid #e5e7eb"
           :border-radius "4px"
           :padding "3px 10px"
           :font-size "12px"
           :cursor "pointer"
           :color "#374151"}}
         label])]
     [:textarea
      {:value code
       :on-change (fn [e]
                    (swap! repl-state assoc :code (.. e -target -value)))
       :on-key-down (fn [e]
                      (when (and (.-metaKey e) (= (.-key e) "Enter"))
                        (.preventDefault e)
                        (eval-code!)))
       :spell-check false
       :style
       {:width "100%"
        :height "180px"
        :font-family mono
        :font-size "13px"
        :padding "10px"
        :border "1px solid #d1d5db"
        :border-radius "6px"
        :resize "vertical"
        :outline "none"
        :line-height "1.6"
        :color "#1f2937"
        :transition "border-color 0.15s, box-shadow 0.15s"}}]
     [:div
      {:style
       {:display "flex"
        :justify-content "space-between"
        :align-items "center"}}
      [:span
       {:style
        {:font-size "12px"
         :color "#9ca3af"
         :font-family mono}}
       "⌘+Enter to evaluate"]
      [:button
       {:on-click (fn [_] (eval-code!))
        :style
        {:background "#4f46e5"
         :color "white"
         :border "none"
         :border-radius "6px"
         :padding "7px 20px"
         :font-size "13px"
         :font-weight "500"
         :cursor "pointer"
         :transition "opacity 0.15s"}}
       "Evaluate"]]
     [result-view {:result result :error error}]]))

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
      {:margin-bottom "20px"}}
     [:h1
      {:style
       {:font-size "20px"
        :font-weight "700"
        :color "#111827"
        :margin-bottom "6px"}}
      "DataScript Playground"]
     [:p
      {:style
       {:font-size "13px"
        :color "#6b7280"
        :display "flex"
        :align-items "center"
        :gap "4px"
        :flex-wrap "wrap"}}
      "Bindings: "
      [code-badge "conn"] " · "
      [code-badge "d/q"] " · "
      [code-badge "d/transact!"] " · "
      [code-badge "d/pull"] " · "
      [code-badge "d/datoms"] " · "
      [code-badge "d/entity"] " · "
      [code-badge "d/touch"]]]
    [:div
     {:style
      {:display "grid"
       :grid-template-columns "1fr 1fr"
       :gap "20px"
       :align-items "start"}}
     [datoms-panel]
     [repl-panel]]]])

(defonce root (atom nil))

(defn ^:dev/after-load re-render []
  (rdom/render @root [app]))

(defn init []
  (reset! root (rdom/create-root (.getElementById js/document "app")))
  (re-render))
