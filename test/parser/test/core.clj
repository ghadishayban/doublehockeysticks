(ns parser.test.core
  (:require [parser.api :as api])
  (:use [clojure.test]))

(deftest valuetest
  (is (= "123" (api/value "123")))
  (is (= "123" (api/value [["123"]])))
  (is (thrown? Exception (api/value [[["123"]]])))
  (is (thrown? Exception (api/value [[[["123"]]]])))
  (is (thrown? Exception (api/value ["123" []])) "This has a second component, though it's empty."))

