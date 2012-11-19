(ns com.shayban.hl7v2.event-stream
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer (pprint)]
            [clojure.string :as str]))

(defn line-vec
  [s]
  (zipmap [:visit :date :event :patient-class] 
          (str/split s #"\t")))

(defn analyze-file [fname] 
  (with-open [f (io/reader fname)]
    (let [visit-streams (->> (line-seq f)
                   (map line-vec)
                   (partition-by :visit))]
      (-> (map (fn [messages]
                 (for [m messages]
                   (map m [:event :patient-class])))
               visit-streams)
        frequencies))))

(defn event-freqs [inf outf] 
  (with-open [x (io/writer outf)]
    (binding [*out* x] 
      (pprint (vec
                (sort-by (comp - second) 
                         (vec (analyze-file inf)))))))) 
