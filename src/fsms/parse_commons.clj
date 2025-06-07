(ns fsms.parse-commons
  (:use [fsms.regex-tools]
        [fsms.commons])
  (:require [clojure.string :as s]))

;; TODO: vocabulary not consistent: final / accepting
(defn parse-final-states 
  "attempts parsing a number of accepting states in a line like
      final z0 z1 z2"
  [line]
  (let [[final & fstates] (re-seq state line)]
    (if (seq fstates)
      {:final-states (set (map second fstates))}
      (assert false (str "PARSE CRITICAL: No accepting states found in line: " line)))))

(defn parse-start 
  "attempts to parse a single start state in a line like
      start z0"
  [line]
  (let [[_ start] (re-find (regex-concat #"^start\s+" state #"$") line)]
    (assert start
            (str "no valid start state found in line: " line))
    {:start start}))

(defn parse-pda-transition
  "attempts parsing a transition from a string such as
      (z0, a, #) -> (z1, A#)
   Lambda in the new stack is replaced by a blank string,
   allowing easy stack modification."
  [line]
  (let [[_ state-from sym tos state-to new-stack :as match]
          (re-find (regex-concat pda-lhs arrow pda-rhs end-of-line)
                   line)]
    (assert match (str "PARSE CRITICAL: not a valid transition: " line))
    (assert (= 1 (count sym)) (str "PARSE CRITICAL: input symbol too long: " sym " in: " line))
    (assert (= 1 (count tos)) (str "PARSE CRITICAL: top of stack too long: " tos " in: " line))
    (assert (not= tos lambda) (str "PARSE CRITICAL: lambda not allowed as top of stack in: " line))
    [{:state state-from, :symbol sym :top-of-stack tos}
     {:state state-to :new-stack (s/replace new-stack lambda "")}]))
