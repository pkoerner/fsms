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

(deftest invalid-transitions-wrong-format
  (testing "invalid transitions in the wrong format are recognised"
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-pda-transition "(")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-pda-transition "(z0, a, a, a) -> ()")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-pda-transition "(z0, a, A) -> (z0, )")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-pda-transition "(z0, a, ) -> (z0, )")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-pda-transition "(z0, a, ) -> (z0, A)")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-pda-transition "(z0, a) -> (z0, A)")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-pda-transition "(z 0, a) -> (z0, A)")))))

(deftest invalid-transitions-logically-wrong
  (testing "the transition format looks right but is rejected"
    (is (thrown-with-msg? AssertionError #"lambda not allowed" 
                          (parse-pda-transition "(z, 0, _) -> (z0, A)")))
    (is (thrown-with-msg? AssertionError #"top of stack too long" 
                          (parse-pda-transition "(z, 0, AA) -> (z0, A)")))  
    (is (thrown-with-msg? AssertionError #"input symbol too long" 
                          (parse-pda-transition "(z, 00, A) -> (z0, A)")))))


(deftest valid-transition
  (testing "valid transitions are parsed correctly"
    (are [x y] (= (parse-pda-transition x) y)
         ;; Note: internally, _ becomes the empty string. This is intended.
         "(z, a, #) -> (z1, _)"
         [{:state "z", :symbol "a", :top-of-stack "#"} {:state "z1" :new-stack ""}]
         "(z ,  a ,#)-> ( z1 ,  _    )"
         [{:state "z", :symbol "a", :top-of-stack "#"} {:state "z1" :new-stack ""}]
         "(z, a, #) -> (z1, A)"
         [{:state "z", :symbol "a", :top-of-stack "#"} {:state "z1" :new-stack "A"}]
         "(z1, _, A) -> (z, AA)"
         [{:state "z1", :symbol "_", :top-of-stack "A"} {:state "z" :new-stack "AA"}])))
