(ns com.shayban.hl7v2.test.core
  (:require [com.shayban.hl7v2.api :as api]
            [com.shayban.hl7v2.escape :as esc])
  (:use [clojure.test]))

(deftest components
  (is (nil? (api/component nil 0)))
  (is (= "asdf" (api/component "asdf" 0)))
  (is (= "bar" (api/component ["asdf" "bar"] 1 0)))
  (is (= "bar" (api/component ["asdf" ["bar"]] 1 0)))
  (is (nil? (api/component ["asdf" nil] 1 0)))
  (is (nil? (api/component [nil nil] 1 0)))
  (is (= "123" (api/component ["123"] 0)))
  (is (= "123" (api/component ["123"] 0)))
  (is (= "123" (api/component ["123"] 0)))
  (is (= "123" (api/component ["1" "123"] 1)))
  (is (nil? (api/component ["1" "123"] 3)))
  (is (= "123" (api/component [["123"]] 0 0)))
  (is (nil? (api/component [["123"]] 0 3)))
  (is (= ["456"] (api/component [["456"] "123"] 0)))
  (is (= "123" (api/component [nil "123"] 1)))
  (is (nil? (api/component [nil "123" nil] 2))))

(deftest escape-seq
  (is (= "\\" (esc/translate [\E] {:escape \\}))))
