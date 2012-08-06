(ns parser.test
  (:require [parser.parse :as p :refer (ASCII_CR)])
  (:import [java.io StringReader]))

(def test-msg
  (str "MSH|^~\\&|AcmeHIS|StJohn|CATH|StJohn|20061019172719||ORM^O01|"
         "|P|2.3" ASCII_CR
         "PID|||20301||Durden^Tyler^~^^Mr.||19700312|M|||88 Punchward Dr.^^Los Angeles^CA^11221^USA|||||||" ASCII_CR
         "PV1||O|OP^^||||4652^Paulson^Robert|||OP|||20061019172717|20061019172718"  ASCII_CR
         "ORC|NW|20061019172719" ASCII_CR
         "OBR|1|200610&19172719||76770^Ultrasound: retroperitoneal^C4|||12349876"))
