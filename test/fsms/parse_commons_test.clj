(ns fsms.parse-commons-test
  (:use [clojure.test]
        [fsms.parse-commons]))

(deftest final-invalid-test
  (testing "invalid accepting state declaration is recognized"
    (is (thrown? AssertionError (parse-final-states "final z0 ### z1")))
    (is (thrown-with-msg? AssertionError #"No accepting states"
                          (parse-final-states "final")))
    (is (thrown-with-msg? AssertionError #"No accepting states"
                          (parse-final-states "final    ")))
    (is (thrown-with-msg? AssertionError #"No accepting states"
                          (parse-final-states "finalz    ")))))

(deftest final-valid-test
  (testing "valid accepting state declaration returns proper values"
    (are [x y] (= (parse-final-states x) y)
         "final z0" {:final-states #{"z0"}}
         "final z0 z1" {:final-states #{"z0" "z1"}}
         "final z_0 z_1" {:final-states #{"z_0" "z_1"}}
         "final z_0z2 z_1" {:final-states #{"z_0z2" "z_1"}})))
