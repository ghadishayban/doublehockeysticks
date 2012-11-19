(ns com.shayban.hl7v2.structure-analysis
  (:require [com.shayban.hl7v2.api :as api]
           [clojure.core.cache :as c]
           [clojure.core.reducers :as r]
           [clojure.java.io :as io]
           [com.shayban.hl7v2.bench :refer (native-xz-input-stream)]
           [com.shayban.hl7v2.codec :refer (hl7-messages)]
           [com.shayban.hl7v2.parse :refer (read-message string-reader)]))

(defn fingerprint
  [m]
  ;; [:event :structure]
  [(-> m
      (api/segment "MSH")
      (api/field 9))
   (api/structure m)])

(def medfile "/tmp/ch/home/cloverleaf/output/HMMInbound.1229.xz")

(defmacro field [seg num] {:segment (name seg) :field num})

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
  (let [caches (r/reduce #(warm-nested-cache %1 (dissoc %2 :value)
                                              (get %2 :value))
                       {}
                       (->> msgs (r/mapcat :segments)
                                (r/mapcat api/field-seq)))]
    (into {}
      (for [[field cache] caches]
        [field (lu-cache-frequency cache)]))))

(defn run-stats [file]
  (with-open [f (native-xz-input-stream file)]
    (stats-all-fields (hl7-messages f))))

(defn probable-field-type
  [stat]
  (let [cnt (count stat)]
    (cond
      (< cnt 2)
      {:field-type :nil-or-constant
       :values stat}
      (>= cnt NUM-ENTRIES)
      {:field-type :unique}
      :else
      {:field-type :dictionary
       :values stat})))

(defn analyze [field-stats]
  (into []
    (for [[fld statistics] field-stats]
      (merge fld (probable-field-type statistics)))))

(defn -main [opt]
  (cond
    (= opt "--help")
    (println "Please specify a filename or - for stdin.  This will write to /tmp/freq")
    :else
    (with-open [output (clojure.java.io/writer "/tmp/freq")]
      (binding [*out* output]
))))


(defmacro super-analyze [f]
  `(with-open [~'f ~f]
     (-> ~'f
         hl7-messages
         stats-all-fields
         analyze)))

(defmacro structure-freq [f]
  `(with-open [~'f ~f]
     (frequencies 
       (map api/structure
         (hl7-messages ~'f)))))
