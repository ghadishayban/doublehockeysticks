(ns parser.parse
  (:refer-clojure :exclude [read])
  (:import (java.io PushbackReader StringReader)))

;;(set! *warn-on-reflection* true)

(def defaultBufSize 3)

;; ASCII codes of character delimiters
(def ASCII_VT (char 11))
(def ASCII_FS (char 28))
(def ASCII_CR (char 13))
(def ASCII_LF (char 10))

;; HL7 Messaging v2.x segment delimite
(def SEGMENT-DELIMITER ASCII_CR)

(defn test-rdr [s]
  (PushbackReader. (StringReader. s)))

(defprotocol Stream
  (read [x])
  (unread [x]))

(deftype Reader [rdr buf pushCnt bufSize pos]
  Stream
  (read [_]
        (if (> @pushCnt 0)
          (let [ch (aget buf (mod (- @pos @pushCnt) bufSize))]
            (do (swap! pushCnt dec)
              ch))
          (do
            (let [int-ch (.read rdr)]
              (if (= int-ch -1)
                nil
                (do 
                  (let [ch (char int-ch)]
                    (aset buf (mod @pos bufSize) ch)
                    (swap! pos inc)
                    ch)))))))
  (unread [_]
         (if (and (< @pushCnt bufSize) (< @pushCnt @pos))
           (swap! pushCnt inc)
           (throw (Exception. "Too much unreading!")))
         nil))

(defn parser [s]
  (Reader. (PushbackReader. (StringReader. s))
           (char-array defaultBufSize)
           (atom 0)
           defaultBufSize
           (atom 0)))

(defn test-msg []
  (parser 
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

(def segdelim \return)

(defn read-delimiters [r]
  (loop [ch (read r)
         delim {} 
         pos 0
         acc []]
      (cond 
        (nil? ch)
        (throw (Exception. "EOF in delimiters"))

        (= ch segdelim)
        (throw (Exception. "Segment ended in delimiters"))

        (= 3 pos)
        (if (= "MSH" (apply str acc))
          (recur (read r)
                  (assoc delim :field ch)
                  (inc pos)
                  nil)
          (throw (Exception. "Header isn't MSH")))

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
            (throw (Exception. "No field delim after delimiters!")))
        
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

(defn read-escaped [r {:keys [escape]}]
  (loop [acc [] ch (read r)]
    (cond
      (nil? ch)
      (throw (Exception. "EOF during escape sequence."))

      (= escape ch)
      ;; ADD ESCAPE SEQUENCE TRANSLATION HERE
      (apply str acc)

      :else
      (recur (conj acc ch) (read r)))))

(defn read-text [r {:keys [escape field component repeating subcomponent]:as delim}]
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
      (recur (conj acc (read-text r delim)) 
             (read r))

      (= ch component)
      (Component. acc)

      (or (= ch field) (= ch SEGMENT-DELIMITER))
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
  (loop [acc [] ch (read r) field-acc []]
    (cond
      (nil? ch)
      (if (seq field-acc)
        (RepeatingField. (conj field-acc (Field. acc)))
        (Field. acc))

      ;;(= ch repeating)

      (or (= ch field) (= ch SEGMENT-DELIMITER))
      (do (unread r) 
        (if (seq field-acc)
          (RepeatingField. (conj field-acc (Field. acc)))
          (Field. acc)))

      (= ch repeating)
      (recur [] 
             (read r) 
             (conj field-acc (Field. acc)))
      
      :else
      (do (unread r)
        (recur (conj acc (read-component r delim)) 
               (read r) 
               field-acc)))))

(defn read-segment-fields [r {:keys [escape field component repeating subcomponent] :as delim}]
  (loop [acc [] ch (read r)]
    (cond
      (nil? ch)
      acc 

      (= ch SEGMENT-DELIMITER)
      (do (unread r) acc)
      
      (= ch field)
        (recur (conj acc (read-field r delim)) (read r)))))

(defn read-segment-header [r {:keys [escape field component repeating subcomponent] :as delim}]
  (let [seg-ID (read-text r delim)]
    (if (or (nil? seg-ID) (not= 3 (count seg-ID)))
      (throw (Exception. "Bad segment header"))
      seg-ID)))

(defn read-segment [r {:keys [escape field component repeating subcomponent] :as delim}]
  (Segment. (read-segment-header r delim)
            (read-segment-fields r delim)))

(defn read-message [r] 
  (let [delim (read-delimiters r)
        msh-fields (read-segment-fields r delim)]
      (loop [acc [(Segment. "MSH" msh-fields )]
                  ch (read r)]
        (cond 
          (nil? ch)
          (Message. delim
                    acc)

          (= ch SEGMENT-DELIMITER)
          (recur (conj acc (read-segment r delim)) (read r))))))

(def r (test-msg))
