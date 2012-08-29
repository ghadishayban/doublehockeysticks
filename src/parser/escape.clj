(ns parser.escape
  (:refer-clojure :exclude [read]))

(defn translate-hex
  [hex]
  (let [sb (StringBuilder.)]
    (doseq [c (map #(apply str %) (partition 2 hex))]
      (.append sb (char (Integer/parseInt c 16))))
    (str sb)))

(defn translate 
  "Many others at http://www.healthintersections.com.au/?page_id=441
  http://www.itscj.ipsj.or.jp/ISO-IR/
  http://wiki.hl7.org/index.php?title=Character_Set_used_in_v2_messages"
  [chars delim]
  (cond
    (= 1 (count chars))
    (case (first chars)
      \E (str (:escape delim))
      \R (str (:repeating delim))
      \T (str (:subcomponent delim))
      \S (str (:component delim))
      \F (str (:field delim))
      nil)
    
    (= [\. \b \r] chars)
    "\n"
    
    (= \X (first chars))
    (translate-hex (rest chars))

    :else
    (throw (Exception. "Escape sequence unimplemented."))))

;; legacy escape sequences.  Use UTF-8 directly instead. It's supported!
;; \\Cxxyy\  Single-byte character set escape sequence with two hexadecimal values not converted
;; \Mxxyyzz\ Multi-byte character set escape sequence with two or three hexadecimal values (zz is optional) not converted

