(ns playground.ui
  (:require
   [clojure.string :as str]
   [datascript.core :as ds]
   [reagent.core :as r]
   ["@codemirror/commands" :refer [defaultKeymap history historyKeymap]]
   ["@codemirror/language" :refer [StreamLanguage defaultHighlightStyle syntaxHighlighting]]
   ["@codemirror/legacy-modes/mode/clojure" :as cm-clojure]
   ["@codemirror/state" :refer [EditorState Prec]]
   ["@codemirror/view" :refer [EditorView keymap]]
   ["@jurjanpaul/codemirror6-parinfer" :refer [parinferExtension]]
   [playground.state :as s]
   [playground.examples :as examples]))

(def mono "ui-monospace, 'Cascadia Code', 'Fira Code', monospace")

(defn schema-panel []
  (let [schema (ds/schema @s/db-state)
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
          (for [label ["Attribute" "Property" "Value"]]
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
         (for [attr attrs
               [k v] (sort (get schema attr))]
           [:tr
            {:key (str attr k)}
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
               :color "#9ca3af"}}
             (str k)]
            [:td
             {:style
              {:padding "4px 8px"
               :font-family mono
               :font-size "12px"
               :color "#374151"}}
             (str v)]])]])]))


(defn datoms-panel []
  (let [db @s/db-state
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
  [:<>
   [:div
    {:style
     {:padding "12px 0"
      :border-bottom "1px solid #f3f4f6"}}
    [code-editor
     {:value code
      :on-change (fn [new-code]
                   (s/update-active-tab!
                     #(update % :cells s/update-cell id
                              (fn [c] (assoc c :code new-code))))
                   (s/ensure-trailing-blank!))
      :on-run (fn [] (s/eval-cell! id))
      :on-format (fn [] (s/format-cell! id))}]]
   [:div
    {:style
     {:display "flex"
      :flex-direction "column"
      :padding "12px 0"
      :border-bottom "1px solid #f3f4f6"}}
    [:button
     {:on-mouse-down (fn [e] (.preventDefault e))
      :on-click (fn [_] (s/eval-cell! id))
      :title "Run (⌘+Enter)"
      :style
      {:background "#4f46e5"
       :color "white"
       :border "none"
       :border-radius "4px"
       :padding "5px 9px"
       :cursor "pointer"
       :font-size "13px"
       :line-height "1"}}
     "▶"]
    [:div {:style {:flex-grow 1}}]
    [:button
     {:on-mouse-down (fn [e] (.preventDefault e))
      :on-click (fn [_] (s/delete-cell! id))
      :title "Delete cell"
      :style
      {:background "transparent"
       :color "#9ca3af"
       :border "none"
       :border-radius "4px"
       :padding "5px 9px"
       :cursor "pointer"
       :font-size "13px"
       :line-height "1"}}
     "🗑"]]
   [:div
    {:style
     {:padding "12px 0"
      :border-bottom "1px solid #f3f4f6"}}
    [:div
     {:style
      {:max-height "20em"
       :overflow-y "auto"}}
     [result-view {:result result :error error}]]]])

(defn tabs-bar []
  (r/with-let
   [template (r/atom "")]
   (let [{:keys [tabs active-tab-id]} @s/tabs-state]
     [:div
      {:style
       {:display "flex"
        :align-items "center"
        :gap "6px"
        :flex-wrap "wrap"}}
      (for [{:keys [id label]} tabs]
        [:div
         {:key (str id)
          :on-click (fn [_] (when (not= id active-tab-id) (s/switch-tab! id)))
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
                                  (s/rename-tab! id new-label))))}
          label]
         (when (> (count tabs) 1)
           [:span
            {:on-click (fn [e]
                         (.stopPropagation e)
                         (s/remove-tab! id))
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
                          (s/add-tab! (when (not= v "blank") (keyword v)))
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
          [:option {:key (str id) :value (name id)} label])]]])))

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

(defn help-panel []
  [:div
   {:style
    {:background "white"
     :border-radius "8px"
     :border "1px solid #e5e7eb"
     :padding "16px 20px"
     :margin-bottom "16px"}}
   [:div
    {:style
     {:display "grid"
      :grid-template-columns "1fr 1fr 1fr"
      :gap "24px"}}
    [:div
     [:h3
      {:style
       {:font-size "11px"
        :font-weight "600"
        :color "#9ca3af"
        :text-transform "uppercase"
        :letter-spacing "0.08em"
        :margin-bottom "8px"}}
      "Environment"]
     [:p
      {:style {:font-size "12px" :color "#374151" :margin "0 0 6px" :line-height "1.6"}}
      "Code runs in a " [:strong "SCI"] " (Small Clojure Interpreter) sandbox — a safe subset of Clojure. Most core fns are available."]
     [:p
      {:style {:font-size "12px" :color "#374151" :margin 0 :line-height "1.6"}}
      "DataScript state persists across cells within a tab. Use " [code-badge "conn"] " to transact or query."]]
    [:div
     [:h3
      {:style
       {:font-size "11px"
        :font-weight "600"
        :color "#9ca3af"
        :text-transform "uppercase"
        :letter-spacing "0.08em"
        :margin-bottom "8px"}}
      "Available Bindings"]
     [:div
      {:style {:display "flex" :flex-wrap "wrap" :gap "4px"}}
      (for [b ["conn" "d/q" "d/transact!" "d/pull" "d/pull-many"
               "d/entity" "d/datoms" "d/touch" "d/db"
               "d/create-conn" "d/empty-db" "d/conn-from-db"
               "update-schema!"]]
        [:span {:key b} [code-badge b]])]]
    [:div
     [:h3
      {:style
       {:font-size "11px"
        :font-weight "600"
        :color "#9ca3af"
        :text-transform "uppercase"
        :letter-spacing "0.08em"
        :margin-bottom "8px"}}
      "Editor"]
     [:p {:style {:font-size "12px" :color "#374151" :margin "0 0 6px"}}
      [code-badge "⌘+Enter"] " — evaluate cell"]
     [:p {:style {:font-size "12px" :color "#374151" :margin "0 0 6px"}}
      [code-badge "Tab"] " — format code (zprint)"]
     [:p {:style {:font-size "12px" :color "#374151" :margin "0 0 6px" :line-height "1.6"}}
      [:strong "Parinfer"] " keeps parentheses balanced as you type — indent to restructure."]
     [:p {:style {:font-size "12px" :color "#374151" :margin 0 :line-height "1.6"}}
      "Double-click a tab label to rename it."]]]])

(defn notebook-panel []
  (let [{:keys [cells]} (s/active-tab)]
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
      [:h2
       {:style
        {:font-size "11px"
         :font-weight "600"
         :color "#9ca3af"
         :letter-spacing "0.08em"
         :text-transform "uppercase"}}
       "Notebook"]
      [:div
       {:style {:display "flex" :gap "8px"}}
       [:button
        {:on-click (fn [_] (s/download-cells!))
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
        {:on-click (fn [_] (s/run-all!))
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
        :grid-template-columns "1fr auto 1fr"
        :column-gap "16px"}}
      [:span
       {:style
        {:font-size "11px"
         :font-weight "500"
         :color "#9ca3af"
         :font-family mono
         :padding-bottom "6px"
         :border-bottom "2px solid #f3f4f6"}}
       "Code"]
      [:div {:style {:border-bottom "2px solid #f3f4f6"}}]
      [:span
       {:style
        {:font-size "11px"
         :font-weight "500"
         :color "#9ca3af"
         :font-family mono
         :padding-bottom "6px"
         :border-bottom "2px solid #f3f4f6"}}
       "Result"]
      (for [{:keys [id] :as cell} cells]
        ^{:key id} [cell-view cell])]]))

(defn app []
  (r/with-let [show-help? (r/atom false)]
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
         :gap "8px"
         :margin-bottom "12px"
         :flex-wrap "wrap"}}
       [:h1
        {:style
         {:font-size "20px"
          :font-weight "700"
          :color "#9ca3af"
          :margin 0}}
        "DataScript Playground"]
       [:button
        {:on-click (fn [_] (swap! show-help? not))
         :title "Toggle help"
         :style
         {:background (if @show-help? "#e0e7ff" "transparent")
          :color (if @show-help? "#4f46e5" "#9ca3af")
          :border (str "1px solid " (if @show-help? "#c7d2fe" "#d1d5db"))
          :border-radius "50%"
          :width "20px"
          :height "20px"
          :font-size "11px"
          :font-weight "700"
          :cursor "pointer"
          :line-height "1"
          :padding 0
          :flex-shrink 0}}
        "?"]
       [:div {:style {:flex 1}}]
       [tabs-bar]]
      (when @show-help?
        [help-panel])
      [:div
       {:style
        {:display "grid"
         :grid-template-columns "28em 1fr"
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
       [notebook-panel]]]]))

