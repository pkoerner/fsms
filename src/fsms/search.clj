(ns fsms.search)

(def MAX-STATES 1000)

(defn build-accept?-fn [init-fn successors-fn accept?-fn discard?-fn]
  (fn [automaton word]
    (loop [nr-states 0
           q (vec (init-fn automaton word))
           seen (set q)]
      (cond (empty? q) false
            (> nr-states MAX-STATES) (do (println "INFO: reached maximum number of states, assuming non-accepting") false)
            (accept?-fn automaton (first q)) true
            (discard?-fn (first q)) (do (println "INFO: reached a limit, discarding configuration" (first q)) false)
            :else
            (let [config (first q)
                  succs (successors-fn automaton config)
                  succs (remove seen succs)]
              (recur (inc nr-states)
                     (into (subvec q 1) succs)
                     (into seen succs)))))))
