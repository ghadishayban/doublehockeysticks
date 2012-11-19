(ns com.shayban.hl7v2.luhn)

(defn luhn [s]
  (let [deltas [0 1 2 3 4 -4 -3 -2 -1 0]
        digits (map #(Character/digit % 10) s)]
    (loop [acc 0 d digits position 0]
      (if (seq d)
        (recur (+ acc (if (odd? position)
                          (+ (first d) (get deltas (first d)))
                          (first d)))
               (rest d)
               (inc position))
        (let [r (mod acc 10)]
          (case r
            0 0
            (- 10 r)))))))

(defn luhn [s]
  (let [deltas [0 1 2 3 4 -4 -3 -2 -1 0]
    digits (map #(Character/digit % 10) s)]
    (loop [acc 0 d digits odd? false]
      (if (seq d)
        (recur (+ acc (if odd?
                          (+ (first d) (get deltas (first d)))
                          (first d)))
               (rest d)
               (not odd?))
        (let [r (mod acc 10)]
          (case r
            0 0
            (- 10 r)))))))



