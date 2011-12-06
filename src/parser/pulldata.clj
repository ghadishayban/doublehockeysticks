;; use string? coll? set?

(defn extract-segments 
  ([{segs :segments} segid]
    (let [xs (filter #(= (:id %) segid) segs)]
      (if (not= (count xs) 1)
        (throw (Exception. "Ambiguous segment specified."))
        (first xs))))
  ([{segs :segments} segid & more]
    (filter #((into #{} (conj more segid)) (:id %)) 
            segs)))

(defn extract
  "Gets the segment, field, component, or subcomponent
  at the specified location.
  If the location is ambiguous, it throws an exception."
  [msg & spec]

  (if (seq spec)
    
    (throw (Exception. "Nothing specified to extract."))
                     
