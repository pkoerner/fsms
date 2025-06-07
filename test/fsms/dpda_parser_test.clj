(ns fsms.dpda-parser-test
  (:use [clojure.test]
        [fsms.dpda-parser]))

(deftest invalid-lines
  (testing "invalid lines are recognised"
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: unexpected input" (parse-line "stupid stuff")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: unexpected input" (parse-line "fin al z0")))))
