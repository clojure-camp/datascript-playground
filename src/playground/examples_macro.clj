(ns playground.examples-macro
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn comment-label [line]
  (let [trimmed (str/trim line)]
    (when (str/starts-with? trimmed ";;")
      (str/trim (subs trimmed 2)))))

;; Code before the first ";;" label is intentionally ignored — use it for
;; setup definitions that should not appear as named examples.
(defn parse-examples [content]
  (loop [[line & rest-lines] (str/split-lines content)
         current-label nil
         current-code []
         examples []]
    (cond
      (nil? line)
      (if current-label
        (conj examples {:label current-label
                        :code (str/trim (str/join "\n" current-code))})
        examples)

      (comment-label line)
      (let [new-label (comment-label line)
            done (if current-label
                   (conj examples {:label current-label
                                   :code (str/trim (str/join "\n" current-code))})
                   examples)]
        (recur rest-lines new-label [] done))

      :else
      (recur rest-lines current-label (conj current-code line) examples))))

(defn filename->label [filename]
  (->> (str/split filename #"[-_]")
       (map str/capitalize)
       (str/join " ")))

(defn file->set [file]
  (let [name (str/replace (.getName file) #"\.cljc$" "")]
    {:id (keyword name)
     :label (filename->label name)
     :examples (parse-examples (slurp file))}))

(defmacro load-example-sets []
  (let [dir (io/file "resources/examples")]
    (->> (seq (.listFiles dir))
         sort
         (filter (fn [f] (str/ends-with? (.getName f) ".cljc")))
         (mapv file->set))))
