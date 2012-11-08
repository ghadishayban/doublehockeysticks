(ns 
  ^{:doc "An HL7 v2 parser.  read-message and string-reader are externally useful fns"}
  parser.parse
  (:refer-clojure :exclude [read])
  (:require [parser.escape :refer (translate)]))

;;(set! *warn-on-reflection* true)

;; TODO: Test ClojureScript version.
;; Does that need a special exception handler?
;; Need to write a StringReader for that.

;; ASCII codes of delimiters
(def ^:const SEGMENT-DELIMITER \return)
(def ^:const LINE-FEED (char 10))

(defprotocol Stream
  "A simple abstraction for reading chars"
  (read [_]))

(defprotocol Hold
  "Simple protocol for pushing back a char"
  (unread [_]))

;; Should I extend-type Reader with mark/reset? What about CLJS?
;; How about peek instead of unread
(deftype StringReader
  [^String s
   pos
   length]
  Stream
  (read [_]
    (if (< @pos length)
      (let [ch (.charAt s @pos)]
        (swap! pos inc)
        ch)))
  Hold
  (unread [_]
    (if (> @pos 0)
      (swap! pos dec))))

(defn string-reader [s]
  (StringReader. s (atom 0) (count s)))

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
(defrecord RepeatingField [fields])

(defn read-escaped [r {:keys [escape] :as delim}]
  (let [delims (conj (set (vals delim)) SEGMENT-DELIMITER)] 
    (loop [acc [] ch (read r)]
      (cond
        (nil? ch)
        (throw (Exception. "EOF during escape sequence."))

        (= escape ch)
        (translate acc delim)

        (contains? delims ch)
        (throw (Exception. "Unexpected delimiter inside escape sequence"))

        :else
        (recur (conj acc ch) (read r))))))

(defn read-text [r {:keys [escape
                           field
                           component
                           repeating
                           subcomponent] :as delim}]
  (loop [acc [] ch (read r)]
    (cond
      (or (= ch field)
          (= ch component)
          (= ch SEGMENT-DELIMITER))
      (do (unread r) (apply str acc))

      (= ch escape)
      (recur (conj acc (read-escaped r delim))
             (read r))

      (or (= ch repeating)
          (= ch subcomponent))
      (do (unread r) (apply str acc))

      (nil? ch)
      (apply str acc)

      :else
      (recur (conj acc ch) (read r)))))

(defn simplify-nesting [coll]
  (case (count coll)
    0 nil
    1 (first coll)
    coll))

(defn read-component [r
                      {:keys [escape
                              field
                              component
                              repeating
                              subcomponent] :as delim}]
  (loop [acc []
         ch (read r)]
    (cond
      (= ch component)
      (simplify-nesting acc)

      (or (= ch field)
          (= ch repeating)
          (= ch SEGMENT-DELIMITER))
      (do (unread r)
          (simplify-nesting acc))

      (= ch subcomponent)
      (recur (conj acc (read-text r delim)) ;; just add the subcomponent
                                            ;; to the vector, no need for
                                            ;; any additional structure
             (read r))

      (nil? ch)
      (simplify-nesting acc)

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
  ;; if the field-acc isn't empty, make a repeating field
  (loop [acc [] ch (read r) field-acc []]
    (cond
      (or (= ch field) (= ch SEGMENT-DELIMITER))
      (do (unread r)
        (if (seq field-acc)
          (RepeatingField. (conj field-acc (simplify-nesting acc)))
          (simplify-nesting acc)))

      ;; When the field repeats, Empty out acc into a new field,
      ;; and place it in field-acc.
      (= ch repeating)
      (recur []
             (read r)
             (conj field-acc (simplify-nesting acc)))

      (nil? ch)
      (if (seq field-acc)
        (RepeatingField. (conj field-acc (simplify-nesting acc)))
        (simplify-nesting acc))

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
  "Parses the HL7 message, returning a nested set of maps
  Parameter r must implement the Stream/Hold protocols." [r]
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
            (if (or (= peek LINE-FEED)
                    (= peek SEGMENT-DELIMITER)  ;; is this line necessary?
                    (nil? peek))
              (Message. delim acc)
              (do (unread r) ;; unread peeked chars
               (recur (conj acc (read-segment r delim)) (read r))))))))))
