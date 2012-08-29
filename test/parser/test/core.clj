(ns parser.test.core
  (:require [parser.api :as api]
            [parser.escape :as esc])
  (:use [clojure.test]))

(deftest values
  (is (= "123" (api/value "123")))
  (is (= nil (api/value nil)))
  (is (= nil (api/value [nil])))
  (is (= "123" (api/value [["123"]])))
  (is (thrown? Exception (api/value [[["123"]]])))
  (is (thrown? Exception (api/value [[[["123"]]]])))
  (is (thrown? Exception (api/value ["123" []])) "This has a second component, though it's empty."))

(deftest components
  (is (= "123" (api/component "123" 0)))
  (is (= "123" (api/component ["123"] 0)))
  (is (= "123" (api/component ["123"] 0 0)))
  (is (= "123" (api/component ["456" "123"] 1)))
  (is (= "123" (api/component [["456"] "123"] 1)))
  (is (= "123" (api/component [nil "123"] 1)))
  (is (nil? (api/component [nil "123" nil] 2)))
  (is (= [] (api/component [nil "123" []] 2))))

(deftest escape-seq
  (is (= "\\" (esc/translate [\E] {:escape \\}))))
