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

(def ^:const NUM-ENTRIES 250)

(defn remember
  [cache val]
  (if (c/has? cache val)
    (c/hit cache val)
    (c/miss cache val nil)))

(defn warm-nested-cache
  [cache-map k val]
  (if-let [cache (get cache-map k)]
    (assoc cache-map k (remember cache val))
    (assoc cache-map k (remember (c/lu-cache-factory {} :threshold NUM-ENTRIES) 
                                 val))))

(defn lu-cache-frequency
  [cache]
  (let [freqs (.-lu cache)] ;; fuck you Java, fields are values
    (into {} (for [k (keys cache)]
               [k (get freqs k)]))))

(defn stats-all-fields [msgs]
  (let [caches (reduce #(warm-nested-cache %1 (dissoc %2 :value)
                                              (get %2 :value))
                       {}
                       (->> msgs (mapcat :segments)
                                (mapcat api/field-seq)))]
    (into {}
      (for [[field cache] caches]
        [field (lu-cache-frequency cache)]))))

(defn -main [opt]
  (cond
    (= opt "--help")
    (println "Please specify a filename or - for stdin.  This will write to /tmp/freq")
    :else
    (with-open [output (clojure.java.io/writer "/tmp/freq")]
      (binding [*out* output]
))))
