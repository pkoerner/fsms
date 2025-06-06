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

(deftest start-invalid-test
  (testing "invalid start state declaration is recognized"
    (is (thrown-with-msg? AssertionError #"no valid start state"
                          (parse-start "start")))
    (is (thrown-with-msg? AssertionError #"no valid start state"
                          (parse-start "start zö")))
    (is (thrown-with-msg? AssertionError #"no valid start state"
                          (parse-start "startz")))
    (is (thrown-with-msg? AssertionError #"no valid start state"
                          (parse-start "start z1 z2")))))

(deftest start-valid-test
  (testing "valid start state declaration returns proper values"
    (are [x y] (= (parse-start x) y)
         "start z0" {:start "z0"}
         "start z_0" {:start "z_0"}
         "start   z_0    " {:start "z_0"})))
