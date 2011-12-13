(ns parser.parse
  (:refer-clojure :exclude [read]))

;; ClojureScript version.
;; Do we need a special exception handler?

;; ASCII codes of character delimiters
(def ASCII_VT \u000B)
(def ASCII_FS \u001C)
(def ASCII_CR \return)
(def ASCII_LF \u000A)

;; HL7 Messaging v2.x segment delimiter
(def SEGMENT-DELIMITER ASCII_CR)

(defprotocol Stream
  "A simple abstraction for reading chars"
  (read [x])
  (unread [x]))

(deftype StringIndexingReader [s next-pos len]
  Stream
  (read [_]
        (if (>= @next-pos len)
          nil
          (let [ch (.charAt s @next-pos)]
            (swap! next-pos inc)
            ch)))
  (unread [_]
          (if (> @next-pos 0)
            (swap! next-pos dec)
            (throw "Can't unread further"))))

 
(defn get-string-reader [s]
  (StringIndexingReader. s 
                         (atom 0) 
                         (.length s)))

(defn test-msg []
  (get-string-reader
    (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
         "|P|2.3" ASCII_CR
         "PID|||20301||Durden^Tyler^~^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||" ASCII_CR
         "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||20061019172717|20061019172718"  ASCII_CR
         "ORC|NW|20061019172719" ASCII_CR
         "OBR|1|200610&19172719||76770^Ultrasound: retroperitoneal^C4|||12349876")))

(defrecord Delimiters [field
                       component
                       repeating
                       escape
                       subcomponent])

(defn read-delimiters [r]
  (loop [ch (read r)
         delim {} 
         pos 0
         acc []]
      (cond 
        (nil? ch)
        (throw "EOF in delimiters")

        (= ch SEGMENT-DELIMITER)
        (throw "Segment ended in delimiters")

        (= 3 pos)
        (if (= "MSH" (apply str acc))
          (recur (read r)
                  (assoc delim :field ch)
                  (inc pos)
                  nil)
          (throw "Header isn't MSH"))

        (= 4 pos)
        (recur (read r)
                (assoc delim :component ch)
                (inc pos)
                nil)
          
        (= 5 pos)
        (recur (read r)
                (assoc delim :repeating ch)
                (inc pos)
                nil)

        (= 6 pos)
        (recur (read r)
                (assoc delim :escape ch)
                (inc pos)
                nil)

        (= 7 pos)
        (recur (read r)
                (assoc delim :subcomponent ch)
                (inc pos)
                nil)

        (= 8 pos)
          (if (= ch (:field delim))
            (do (unread r) (map->Delimiters delim))
            (throw "No field delim terminating delimiters."))
        
        :else
        (recur (read r)
               delim
               (inc pos)
               (conj acc ch)))))


(defrecord Message [delimiters segments])
(defrecord Segment [id fields])
(defrecord Field [components])
(defrecord Component [subcomponents])
(defrecord RepeatingField [fields])

(defn read-escaped [r {:keys [escape] :as delim}]
  (loop [acc [] ch (read r)]
    (cond
      (nil? ch)
      (throw "EOF during escape sequence.")

      (= escape ch)
      ;; TODO: Add escape sequence translation here,
      ;; pass delimiters to translation helper.
      (apply str acc)

      :else
      (recur (conj acc ch) (read r)))))

(defn read-text [r {:keys [escape 
                           field 
                           component 
                           repeating 
                           subcomponent] :as delim}]
  (loop [acc [] ch (read r)]
    (cond
      (nil? ch)
      (do (apply str acc))

      (= ch escape)
      (recur (conj acc (read-escaped r delim))
             (read r))

      (or (= ch field)
          (= ch component)
          (= ch repeating)
          (= ch subcomponent)
          (= ch SEGMENT-DELIMITER))
      (do (unread r) (apply str acc))
      
      :else 
      (recur (conj acc ch) (read r)))))

(defn read-component [r 
                      {:keys [escape 
                              field 
                              component 
                              repeating 
                              subcomponent] :as delim}]
  (loop [acc [] 
         ch (read r)]
    (cond
      (nil? ch)
      (Component. acc)

      (= ch subcomponent)
      (recur (conj acc (read-text r delim)) ;; just add the subcomponent
                                            ;; to the vector, no need for
                                            ;; and additional structure
             (read r))

      (= ch component) 
      (Component. acc)

      (or (= ch field) 
          (= ch repeating)
          (= ch SEGMENT-DELIMITER))
      (do (unread r) (Component. acc))
      
      :else
      (do (unread r)
        (recur (conj acc (read-text r delim)) 
               (read r))))))

(defn read-field [r {:keys [escape 
                            field
                            component 
                            repeating 
                            subcomponent] :as delim}]
  ;; field-acc helps to handle repeating fields.
  ;; acc is for the components within a single field.
  (loop [acc [] ch (read r) field-acc []]
    (cond

      ;; if the field-acc isn't empty, make a repeating field
      (nil? ch)
      (if (seq field-acc)
        (RepeatingField. (conj field-acc (Field. acc)))
        (Field. acc))

      (or (= ch field) (= ch SEGMENT-DELIMITER))
      (do (unread r) 
        (if (seq field-acc)
          (RepeatingField. (conj field-acc (Field. acc)))
          (Field. acc)))

      ;; When the field repeats, Empty out acc into a new field,
      ;; and place it in field-acc.
      (= ch repeating)
      (recur [] 
             (read r) 
             (conj field-acc (Field. acc)))
      
      :else
      (do (unread r)
        (recur (conj acc (read-component r delim)) 
               (read r) 
               field-acc)))))

(defn read-segment-fields [r {:keys [escape
                                     field 
                                     component 
                                     repeating 
                                     subcomponent] :as delim}]
  (loop [acc [] ch (read r)]
    (cond
      (nil? ch)
      acc 

      (= ch SEGMENT-DELIMITER)
      (do (unread r) acc)
      
      (= ch field)
        (recur (conj acc (read-field r delim)) (read r)))))

(defn read-segment-header
  "This returns the three character label on the Segment.
  It calls read-text, so it can potentially handle escape
  sequences.  Not a good idea."
  [r {:keys [escape 
             field 
             component 
             repeating 
             subcomponent] :as delim}]
  (let [seg-ID (read-text r delim)]  ;; FIXME: read-text can parse escapes
    (if (or (nil? seg-ID) (not= 3 (count seg-ID)))
      (throw "Bad segment header")
      seg-ID)))

(defn read-segment 
  [r {:keys [escape 
             field 
             component 
             repeating 
             subcomponent] :as delim}]
  (Segment. (read-segment-header r delim)
            (read-segment-fields r delim)))

(defn read-message
  "Parses the HL7 message.
  Parameter r must implement the Reader protocol." [r] 
  (let [delim (read-delimiters r)
        msh-fields (vec (concat [nil delim]
                                (read-segment-fields r delim)))]
      (loop [acc [(Segment. "MSH" msh-fields )]
                  ch (read r)]
        (cond 
          (nil? ch)
          (Message. delim
                    acc)

          ;; FIXME: Also terminate reading upon encountering End Block character(s)
          (= ch SEGMENT-DELIMITER)
          (let [peek (read r)]  ;; peek to see if end of message
            (if (or (= peek ASCII_LF)
                    (= peek SEGMENT-DELIMITER)  ;; is this line necessary?
                    (nil? peek))
              (Message. delim acc)
              (do (unread r) ;; unread peeked chars
               (recur (conj acc (read-segment r delim)) (read r)))))))))

(comment (def r (test-msg)))
