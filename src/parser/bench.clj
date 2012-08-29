(ns parser.bench
  (:gen-class)
  (:require [parser.parse :as p]
            [parser.codec :refer (hl7-msg-seq)]
            [clojure.java.io :as io]))

(defn native-xz-input-stream [fname & args]
  (let [{binpath :binpath
         :or {binpath "xzcat" :buffer-size 200000}} (apply hash-map args)
        command-args [binpath fname]
        process (.exec (Runtime/getRuntime)
                  ^"[Ljava.lang.String;" (into-array command-args))]
    (apply io/reader (.getInputStream process) args)))

(defn parse-all-in-file [path]
  (map (comp p/read-message p/string-reader)
       (-> System/in
         (io/reader :buffer-size (bit-shift-left 1 21))
         hl7-msg-seq)))

(defn -main [path]
  (parse-all-in-file path))
