(ns fsms.dpda-parser-test
  (:use [clojure.test]
        [fsms.dpda-parser]))


(deftest invalid-lines
  (testing "invalid lines are recognised"
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: unexpected input" (parse-line "stupid stuff")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: unexpected input" (parse-line "fin al z0")))))

(deftest invalid-transitions-wrong-format
  (testing "invalid transitions in the wrong format are recognised"
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-transition "(")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-transition "(z0, a, a, a) -> ()")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-transition "(z0, a, A) -> (z0, )")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-transition "(z0, a, ) -> (z0, )")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-transition "(z0, a, ) -> (z0, A)")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-transition "(z0, a) -> (z0, A)")))
    (is (thrown-with-msg? AssertionError #"PARSE CRITICAL: not a valid transition" 
                          (parse-transition "(z 0, a) -> (z0, A)")))))

(deftest invalid-transitions-logically-wrong
  (testing "the transition format looks right but is rejected"
    (is (thrown-with-msg? AssertionError #"lambda not allowed" 
                          (parse-transition "(z, 0, _) -> (z0, A)")))
    (is (thrown-with-msg? AssertionError #"top of stack too long" 
                          (parse-transition "(z, 0, AA) -> (z0, A)")))  
    (is (thrown-with-msg? AssertionError #"input symbol too long" 
                          (parse-transition "(z, 00, A) -> (z0, A)")))))


(deftest valid-transition
  (testing "valid transitions are parsed correctly"
    (are [x y] (= (parse-transition x) y)
         ;; Note: internally, _ becomes the empty string. This is intended.
         "(z, a, #) -> (z1, _)"
         [{:state "z", :symbol "a", :top-of-stack "#"} {:state "z1" :new-stack ""}]
         "(z ,  a ,#)-> ( z1 ,  _    )"
         [{:state "z", :symbol "a", :top-of-stack "#"} {:state "z1" :new-stack ""}]
         "(z, a, #) -> (z1, A)"
         [{:state "z", :symbol "a", :top-of-stack "#"} {:state "z1" :new-stack "A"}]
         "(z1, _, A) -> (z, AA)"
         [{:state "z1", :symbol "_", :top-of-stack "A"} {:state "z" :new-stack "AA"}])))
