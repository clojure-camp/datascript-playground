(ns playground.core
  (:require
   [reagent.dom.client :as rdom]
   [playground.state :as s]
   [playground.ui :as ui]))

(defonce root (atom nil))

(defn ^:dev/after-load re-render []
  (rdom/render @root [ui/app]))

(defn init []
  (s/load-saved-state!)
  (reset! root (rdom/create-root (.getElementById js/document "app")))
  (re-render))
