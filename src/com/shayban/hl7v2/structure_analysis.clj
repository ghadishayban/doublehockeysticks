(ns com.shayban.hl7v2.structure-analysis
  (:require [com.shayban.hl7v2.api :as api]
           [clojure.core.cache :as c]
           [clojure.core.reducers :as r]
           [clojure.java.io :as io]
           [com.shayban.hl7v2.bench :refer (native-xz-input-stream)]
           [com.shayban.hl7v2.output-stats :refer (spit-stats)]
           [com.shayban.hl7v2.codec :refer (hl7-messages)]
           [com.shayban.hl7v2.parse :refer (*unescape* read-message string-reader)]))

(defn fingerprint
  [m]
  ;; [:event :structure]
  [(-> m
      (api/segment "MSH")
      (api/field 9))
   (api/structure m)])

(defn ->stream
  [option]
  (cond
    (= option "-")
    (clojure.java.io/reader System/in :buffer-size 1024000)
    (.endsWith option ".xz")
    (native-xz-input-stream option)
    :else
    (clojure.java.io/reader option)))

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
  (let [freqs (.-lu cache)] ;; pull out internal cache hit stats
    (into {} (for [k (keys cache)]
               [k (get freqs k)]))))

(defn collect-field-stats [msgs]
  (let [caches (r/reduce #(warm-nested-cache %1 (dissoc %2 :value)
                                              (get %2 :value))
                       {}
                       (->> msgs (r/mapcat :segments)
                                (r/mapcat api/field-seq)))]
    (into {}
      (for [[field cache] caches]
        [field (lu-cache-frequency cache)]))))

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

(defn interpret-stats [field-stats]
  (into []
    (for [[fld statistics] field-stats]
      (merge fld (probable-field-type statistics)))))

(defmacro analyze-stream [f]
  `(with-open [f# ~f]
     (binding [*unescape* false]
       (-> f#
           hl7-messages
           collect-field-stats
           interpret-stats))))

(defn -main
  [filespec diroutput]
  (cond
    (= filespec "--help")
    (println "Please specify a filename or - for stdin, followed by a directory for analysis output.")
    :else
    (do 
      (println "Working...")
      (spit-stats (analyze-stream (->stream filespec))
                diroutput)
      (println "Done!"))))
