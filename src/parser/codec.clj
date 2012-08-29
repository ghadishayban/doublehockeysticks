(ns parser.codec
  (:refer-clojure :exclude [read-line])
  (:import [java.io BufferedReader]))

(defn read-line [^BufferedReader rdr]
  (let [c (.read rdr)]
    (when (not= c -1)
      (let [sb (StringBuilder. 8192)]
        (loop [ch c]
          (if (or (= ch -1)
                  (= ch 10))
            (str sb)
            (do (.append sb (char ch))
                (recur (.read rdr)))))))))

(defn hl7-msg-seq [^BufferedReader rdr]
  (when-let [line (read-line rdr)]
    (cons line (lazy-seq (hl7-msg-seq rdr)))))

