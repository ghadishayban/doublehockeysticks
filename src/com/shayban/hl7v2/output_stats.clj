(ns com.shayban.hl7v2.output-stats
  (:require [clojure-csv.core :as csv]))

(defn stat-to-string
  [val]
  (cond
    (map? val)
    "Some sort of map"
    (coll? val)
    (map str (flatten val))
    :else
    (vector (str val))))

(defn dictionary-to-str [field]
  (csv/write-csv (map stat-to-string
                      (keys (:values field)))))

(defn field-friendly [f]
  [(:segment f)
   (str (:field f))])

(defmulti field-to-cell :field-type)

(defmethod field-to-cell
  :nil-or-constant
  [field]
  (concat (field-friendly field)
          ["Empty or constant"]
          (stat-to-string (keys (:values field)))))

(defmethod field-to-cell
  :dictionary
  [field]
  (concat (field-friendly field)
          ["Dictionary/Table"]))

(defmethod field-to-cell
  :unique
  [field]
  (concat (field-friendly field)
          ["Unique"]))

(defn spit-stats [stats dir]
  (let [sortedstats (sort-by #(mapv % [:segment :field]) stats)]
    (println "writing stats")
    (spit (str dir "/" "stats.csv")
          (csv/write-csv (map field-to-cell sortedstats)))
    (println "writing dictionaries")
    (doseq [dict-f (filter #(= :dictionary (:field-type %)) stats)]
      (let [filename (format "%s/%s-%s-dict.csv"
                             dir
                             (:segment dict-f)
                             (str (:field dict-f)))]
        (spit filename (dictionary-to-str dict-f))))))
