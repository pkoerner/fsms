(ns fsms.parse-commons
  (:use [fsms.regex-tools]))

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
  (let [[_ start] (re-find (regex-concat #"^start" state #"$") line)]
    (assert start
            (str "no valid start state found in line: " line))
    {:start start}))

