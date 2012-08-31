(ns ^{:doc "Helpers for pulling out raw messages as strings from piles or streams"}
  parser.codec
  (:refer-clojure :exclude [line-seq read-line])
  (:require [parser.parse :as p])
  (:import [java.io BufferedReader]))

(defn read-line
  "Just like Reader/readLine but only splits on newline, not CR."
  [^BufferedReader rdr]
  (let [c (.read rdr)]
    (when (not= c -1)
      (let [sb (StringBuilder. 8192)]
        (loop [ch c]
          (if (or (= ch -1)
                  (= ch 10))
            (str sb)
            (do (.append sb (char ch))
                (recur (.read rdr)))))))))

(defn line-seq [^BufferedReader rdr]
  (when-let [line (read-line rdr)]
    (cons line (lazy-seq (line-seq rdr)))))

(defn hl7-messages [rdr]
  (map (comp p/read-message p/string-reader)
       (line-seq rdr))) 
