(ns parser.api)

(defn structure
  [m]
  (map :id (:segments m)))

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
  "Takes a segment and returns a field from it, or nil if it field
   doesn't exist. N.B. field-num is a one-based index.  If the field
   is repeating, this returns the first repetition only."
  [segment field-num]
  (let [idx (dec field-num)]
    (if (< idx (count (:fields segment)))
      (let [fld (nth (:fields segment) idx)]
        (if (:fields fld)
          (first (:fields fld))
          fld)))))

(defn repeating-field
  "Note, id is a one-based field index, whereas repetition is 0-based"
  [segment field-num]
  (let [idx (dec field-num)]
    (if (< idx (count (:fields segment)))
      (let [fld (nth (:fields segment) idx)]
        (or (:fields fld) fld)))))

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
  ([field idx]
    (if (< idx (count field))
      (nth field idx)))
  ([field idx sub-idx]
    (let [subcomps (component field idx)]
      (cond
        (vector? subcomps)
        (if (< sub-idx (count subcomps))
          (nth subcomps sub-idx))
        (= 0 sub-idx)
        subcomps
        :else
        (throw (Exception. "Unknown field structure"))))))
