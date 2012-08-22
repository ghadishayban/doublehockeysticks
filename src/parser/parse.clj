(ns parser.parse
  (:refer-clojure :exclude [read])
  (:import (java.io Reader StringReader)))

;;(set! *warn-on-reflection* true)

;; TODO: Test ClojureScript version.
;; Does that need a special exception handler?
;; Need to write a StringReader for that.

(def defaultBufSize 3)

;; ASCII codes of character delimiters
(def ASCII_VT (char 11))
(def ASCII_FS (char 28))
(def ASCII_CR \return)
(def ASCII_LF (char 10))

;; HL7 Messaging v2.x segment delimiter
(def SEGMENT-DELIMITER ASCII_CR)

(defprotocol Stream
  "A simple abstraction for reading chars"
  (read [_]))

(defprotocol Hold
  (unread [_]))

;; Does the following reek of being imperative? Yah.
;; Should I extend-type Reader with mark/reset? What about CLJS?
(deftype PushbackReader
  ^{:doc "Similar to java.io.PushbackReader, except that you don't have to
         specify what to push back.  Also, it returns chars, not ints.
         rdr is the underlying Reader, buf is a primitive char array,
         pushCnt tracks how many chars have been pushed back into the
         stream, bufSize is the size of the primitive array, and pos
         is the position within buf that should be read."
    :private true}
  [^java.io.Reader rdr
   ^chars buf
   pushCnt
   bufSize
   pos]
  Stream
  (read [_]
        (cond
          (> @pushCnt 0)
          ;; pos can be greater than bufSize, so mod it.
          (let [ch (aget buf (mod (- @pos @pushCnt) bufSize))]
            (swap! pushCnt dec)
            ch)
          (= @pushCnt 0)
          (let [int-ch (.read rdr)]
            (if (= int-ch -1)
              nil
              (do
                (let [ch (char int-ch)]
                  (aset buf (mod @pos bufSize) ch)
                  (swap! pos inc)
                  ch))))
          :else
          (throw (Exception. "Too much unreading!"))))
  Hold
  (unread [_]
         (if (and (< @pushCnt bufSize)
                  (< @pushCnt @pos)) ; we're not at the beginning
           (swap! pushCnt inc)
           (throw (Exception. "Too much unreading!")))
         nil))

(defn pushback-reader
  "Constructs a pushbackreader from a java.io.Reader"
  [rd]
  (PushbackReader. rd
           (char-array defaultBufSize)
           (atom 0)
           defaultBufSize
           (atom 0)))

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
        nil ;; instead of throwing up

        (= ch SEGMENT-DELIMITER)
        (throw (Exception. "MSH segment ended early with a segment delimiter"))

        (= 3 pos)
        (if (= "MSH" (apply str acc))
          (recur (read r)
                  (assoc delim :field ch)
                  (inc pos)
                  nil)
          (throw (Exception. "Header doesn't begin with MSH")))

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
            (throw (Exception. "No field delim immediately after delimiters.")))

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
      (throw (Exception. "EOF during escape sequence."))

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
      (throw (Exception. "Bad segment header"))
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
  (if-let [delim (read-delimiters r)]    ;; bail out if the delimiters are null
    (let [msh-fields (vec (concat [nil delim]
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
               (recur (conj acc (read-segment r delim)) (read r))))))))))
