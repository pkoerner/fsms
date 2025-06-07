(ns fsms.dpda-test
  (:use [fsms.dpda]
        [clojure.test]))

(deftest initial-configurations-test
  (testing "the initial configuration (singular) of the DPDA is correct"
    (are [x y z] (= x (initial-configurations y z))
         [{:state "z0" :input "foo" :stack "#"}]
         {:start "z0"} "foo"
         [{:state "z_0" :input "foo" :stack "#"}]
         {:start "z_0"} "foo"
         [{:state "z_0" :input "a" :stack "#"}]
         {:start "z_0"} "a"
         [{:state "z_0" :input "" :stack "#"}]
         {:start "z_0"} "")))

(deftest next-states-test
  (testing "the successor configurations are calculated correctly"
    (are [dpda config succs] (= (set succs) (set (next-states dpda config)))
         ;; wrong state
         {:delta {{:state "z1", :symbol "0", :top-of-stack "#"} {:state "z1", :new-stack "A#"}}}
         {:state "z0", :input "000", :stack "#"}
         #_=> []

         ;; wrong symbol
         {:delta {{:state "z0", :symbol "1", :top-of-stack "#"} {:state "z1", :new-stack "A#"}}}
         {:state "z0", :input "000", :stack "#"}
         #_=> []

         ;; correct state, pushing a symbol
         {:delta {{:state "z0", :symbol "0", :top-of-stack "#"} {:state "z1", :new-stack "A#"}}}
         {:state "z0", :input "000", :stack "#"}
         #_=> [{:state "z1", :input "00", :stack "A#"}]

         ;; correct state, replacing a symbol
         {:delta {{:state "z0", :symbol "0", :top-of-stack "A"} {:state "z1", :new-stack ""}}}
         {:state "z0", :input "000", :stack "A#"}
         #_=> [{:state "z1", :input "00", :stack "#"}]

         ;; correct state, replacing last symbol
         {:delta {{:state "z0", :symbol "0", :top-of-stack "#"} {:state "z1", :new-stack ""}}}
         {:state "z0", :input "000", :stack "#"}
         #_=> [{:state "z1", :input "00", :stack ""}]

         ;; lambda transition
         {:delta {{:state "z0", :symbol "_", :top-of-stack "A"} {:state "z1", :new-stack "A"}}}
         {:state "z0", :input "000", :stack "A#"}
         #_=> [{:state "z1", :input "000", :stack "A#"}]

         ;; lambda transition
         {:delta {{:state "z0", :symbol "_", :top-of-stack "#"} {:state "z1", :new-stack ""}}}
         {:state "z0", :input "000", :stack "#"}
         #_=> [{:state "z1", :input "000", :stack ""}]

         ;; two transitions applicable -- can't be, only after refactoring
         )))

(deftest dpda-accepting-test
  (testing "configurations are accepted"
    (is (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "", :stack ""}))
    (is (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "", :stack "#"}))
    (is (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "", :stack "ABC"}))
    (is (dpda-accepting-configuration? {:final-states #{"z1"}} {:state "z1", :input "", :stack "ABC"}))))

(deftest not-accepting-test
  (testing "configurations are not accepted"
    (is (not (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z1", :input "", :stack ""})))
    (is (not (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z1", :input "", :stack "#"})))
    (is (not (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "a", :stack ""})))
    (is (not (dpda-accepting-configuration? {:final-states #{"z0"}} {:state "z0", :input "a", :stack "#"})))))
