(ns com.shayban.hl7v2.test.luhn
  (:require [com.shayban.hl7v2.luhn :refer (luhn)]
            [clojure.java.io :as io])
  (:use clojure.test)
  (:import java.io.PushbackReader))

(def url "com/shayban/hl7v2/test/luhn_data.clj")

(deftest loonie
  (testing "strings are equiv"
    (with-open [pbr (PushbackReader. (io/reader (io/resource url)))]
      (binding [*read-eval* false]
        (loop [[visit checkdigit] (read pbr false nil)]
          (when (and visit checkdigit)
            (is (= checkdigit (str (luhn visit))))
            (recur (read pbr false nil))))))))
