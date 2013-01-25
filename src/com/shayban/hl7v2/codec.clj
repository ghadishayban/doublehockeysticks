(ns ^{:doc "Helpers for pulling out raw messages as strings from piles or streams"}
  com.shayban.hl7v2.codec
  (:refer-clojure :exclude [line-seq read-line])
  (:require [com.shayban.hl7v2.parse :as p]
            [clojure.core.reducers :as r])
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
  (->> (line-seq rdr)
    (r/map (fn [s]
             (try (p/read-message (p/string-reader s))
               (catch Exception e (spit (format "/tmp/invalid-%s.hl7"
                                                (java.util.UUID/randomUUID))
                                        s)))))
    (r/filter identity)))
