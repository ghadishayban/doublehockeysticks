(ns ^{:doc "Helpers for getting translating HL7 escape sequences"}
  com.shayban.hl7v2.escape)

;; TODO translate-hex for clojurescript

(defn translate-hex
  "Takes a vec of hex Chars and returns a string"
  [hex]
  (let [sb (StringBuilder.)]
    (doseq [c (map #(apply str %) (partition 2 hex))]
      (.append sb (char (Integer/parseInt c 16))))
    (str sb)))

(defn translate 
  "Handle the most common escape sequences.
   Use UTF-8 directly instead. It's supported!
   Many others at http://www.healthintersections.com.au/?page_id=441
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
    
    (or (= [\. \b \r] chars)
        (= [\. \B \R] chars))
    "\n"
    
    (= \X (first chars))
    (translate-hex (rest chars))

    :else
    (throw (Exception. "Escape sequence unimplemented."))))

;; legacy escape sequences.
;; \\Cxxyy\  Single-byte character set escape sequence with two hexadecimal values not converted
;; \Mxxyyzz\ Multi-byte character set escape sequence with two or three hexadecimal values (zz is optional) not converted
