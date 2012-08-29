(ns parser.structure-analysis
  (:gen-class)
  (:require [parser.api :as api]
           [parser.codec :refer (hl7-msg-seq)]
           [parser.parse :refer (read-message string-reader)]))

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
    :else
    (clojure.java.io/reader option)))

(defn structure-frequency
  [reader]
  (with-open [stream reader]
    (frequencies (map (comp fingerprint
                            read-message
                            string-reader)
                      (hl7-msg-seq stream)))))

(defn raw-frequency
  [streamname]
  (-> streamname ->stream
      structure-frequency))

(defn event-freq
  [struct-freq]
  (for [[[sig struct] v] struct-freq]
    {:event sig
     :structure (vec struct)
     :frequency v}))

(defn -main [opt]
  (cond
    (= opt "--help")
    (println "Please specify a filename or - for stdin.  This will write to /tmp/freq")
    :else
    (with-open [output (clojure.java.io/writer "/tmp/freq")]
      (binding [*out* output]
       (prn (event-freq (raw-frequency opt)))))))
