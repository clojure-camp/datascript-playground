(ns playground.examples
  (:require-macros
   [playground.examples-macro :refer [load-example-sets]]))

;; Must "touch" this file so that shadow-cljs picks up changes in the examples

(def sets (load-example-sets))
