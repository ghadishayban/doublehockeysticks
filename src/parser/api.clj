;; use string? coll? set?
(ns parser.api)

(defn structure
  [m]
  (map :id (:segments m)))

{:segment 
 :field
 :component
 :subcomponent}

;;get segment DONE
;;get segments DONE
;;get field from segment DONE
;;get specific repeating field DONE
;;comp/subcomp DONE
;;have an api that doesn't throw array out of bounds

(defn segment-name-pred
  [name]
  (fn [seg]
    (= (:id seg) name)))

(defn all-segments
  [msg segment-id]
  (filter (segment-name-pred segment-id) (:segments msg)))

(defn segment
  [msg segment-id]
  (-> msg
    (all-segments segment-id)
    first))

;; (field 3)
;; (field 3 :rep 1)
;; or (repeating-field 2 :rep 1)
;; (component 2)
;; (subcomponent 2)
(defn field
  "Note, field id is a one-based index"
  [segment id]
  (let [fld (nth (:fields segment) (dec id))]
    (if (:fields fld)
      (first (:fields fld))
      fld)))

(defn repeating-field
  "Note, id is a one-based field index, whereas repetition is 0-based"
  ([segment id]
    (-> (:fields segment)
        (nth (dec id))
        :fields))
  ([segment id repetition]
    (nth (repeating-field segment id) repetition)))

(defn value
  [field]
  (loop [val field nest-level 2]
    (cond
      (or (string? val)
          (nil? val))
      val
      (sequential? val)
      (if (= 1 (count val))
        (if (pos? nest-level)
          (recur (first val) (dec nest-level))
          (throw (Exception. "Strangely nested field")))
        (throw (Exception. "Not a simple field")))
      :else
      (throw (Exception. "Unknown field structure")))))

(defn component
  ([field comp]
    (cond
      (vector? field)
      (nth field comp)
      (= 0 comp)
      field)) 
  ([field comp sub]
    (component (component field comp) ;; Ugly?
               sub)))
  
