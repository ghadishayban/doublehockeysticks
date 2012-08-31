(ns parser.structure-analysis
  (:gen-class)
  (:require [parser.api :as api]
           [clojure.core.cache :as c]
           [parser.bench :refer (native-xz-input-stream)]
           [parser.codec :refer (hl7-messages)]
           [parser.parse :refer (read-message string-reader)]))

(defn fingerprint
  [m]
  ;; [:event :structure]
  [(-> m
      (api/segment "MSH")
      (api/field 9))
   (api/structure m)])

(def medfile "/tmp/ch/home/cloverleaf/output/HMMInbound.1229.xz")

(defn ->stream
  [option]
  (cond
    (= option "-")
    (clojure.java.io/reader System/in :buffer-size 1024000)
    :else
    (clojure.java.io/reader option)))

(defn in-hl7-stream
  [reader]
  (with-open [stream reader]
    (frequencies (map (comp fingerprint
                            read-message
                            string-reader)
                      (hl7-messages stream)))))

(defn accumulate-cache [cache f coll]
  (reduce
    (fn [cache msg]
      (let [val (f msg)]
        (if (c/has? cache val)
          (c/hit cache val)
          (c/miss cache val nil))))
    cache
    coll))

;; RXE 2.1
(defn drugname [m]
   (-> m (api/segment "RXE")
         (api/field 2)
         (api/component 1)))

(defn lu-cache-frequency
  [cache]
  (let [freqs (.-lu cache)] ;; fuck you Java, fields are values
    (into {} (for [k (keys cache)]
               [k (get freqs k)]))))

(defn rough-val-frequency [f n coll]
  (let [cache (c/lu-cache-factory {} :threshold n)]
    (-> cache
        (accumulate-cache f coll)
        lu-cache-frequency)))

(defn drug-frequency [file n]
  (with-open [b (native-xz-input-stream file)]
    (->> (hl7-messages b)
         (rough-val-frequency drugname n))))

(defn -main [opt]
  (cond
    (= opt "--help")
    (println "Please specify a filename or - for stdin.  This will write to /tmp/freq")
    :else
    (with-open [output (clojure.java.io/writer "/tmp/freq")]
      (binding [*out* output]
))))
