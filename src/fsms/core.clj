(ns fsms.core
  (:require [fsms.dpda-parser :as dpda-parser]
            [fsms.dpda :as dpda]
            [fsms.turing-machine :as tm]
            [fsms.turing-parser :as tm-parser]
            [fsms.config :as config]
            [fsms.search :refer [build-accept?-fn *debug*]]
            [fsms.cli :as cli]
            [clojure.java.io :as io])
  (:gen-class))




(defn validate-automaton [accept?-fn automaton config]
  (let [err1 (for [word (:accept config)
                   :when (not (accept?-fn automaton word))]
               (str "Word '" word "' should have been accepted, but was rejected"))
        err2 (for [word (:reject config)
                   :when (accept?-fn automaton word)]
               (str "Word '" word "' should have been rejected, but was accepted"))]
    (concat err1 err2)))

(defn validate-calculations [accept?-fn automaton config result-fn]
  (for [[input output] config
        :let [res (result-fn (accept?-fn automaton input))]
        :when (not= res output)]
    (str "Input " input " should yield '" output "' but was '" res "' instead.")))

(defn validate-dpda [file config]
  (let [dpda (dpda-parser/file->dpda file)
        config (config/load-config config)]
    (validate-automaton (build-accept?-fn dpda/initial-configurations
                                          dpda/next-states
                                          dpda/accepting-configuration?
                                          dpda/discard-config?)
                        dpda
                        config)))

(defn validate-tm [file config]
  (let [tm (tm-parser/file->tm file)
        config (config/load-config config)]
    (validate-automaton (build-accept?-fn tm/initial-configurations
                                          tm/turing-step
                                          tm/turing-accepting?
                                          tm/turing-discard?)
                        tm
                        config)))

(defn validate-dtm [file config]
  (let [tm (tm-parser/file->tm file)
        config (config/load-config config)]
    (tm/assert-deterministic tm)
    (validate-automaton (build-accept?-fn tm/initial-configurations
                                          tm/turing-step
                                          tm/turing-accepting?
                                          tm/turing-discard?)
                        tm
                        config)))

(defn validate-calc-dtm [file config]
  (let [tm (tm-parser/file->tm file)
        config (config/load-config config)]
    (tm/assert-deterministic tm)
    (validate-calculations (build-accept?-fn tm/initial-configurations
                                             tm/turing-step
                                             tm/turing-accepting?
                                             tm/turing-discard?)
                           tm
                           config
                           tm/result-from-configuration)))


(defn execute-with-output [f args opts]
  (binding [*out* (if (:output opts)
                    (io/writer (:output opts))
                    *out*)
            *debug* (if (:debug opts) true *debug*)]
    (let [res (apply f args)]
      (doseq [l res]
        (println (str "; " l)))
      (if (seq res)
        (println 0)
        (println (:score opts))))))


(defn -main [& args]
  (let [{:keys [action args options exit-message ok?]} (cli/validate-args args)]
    (if exit-message
      (cli/exit (if ok? 0 1) exit-message)
      (case action
        "check-dpda" (execute-with-output validate-dpda args options)
        "check-tm"   (execute-with-output validate-tm args options)
        "check-dtm"  (execute-with-output validate-dtm args options)
        "check-calc-dtm" (execute-with-output validate-calc-dtm args options)
        ))))


(comment
  (def dpda (dpda-parser/file->dpda (io/resource "dpda2.edn")))
  (dpda/initial-configurations dpda "abc")
  (dpda/next-states dpda (first (dpda/next-states dpda  (first (dpda/initial-configurations dpda "abc")))))

  ((build-accept?-fn dpda/initial-configurations
                     dpda/next-states
                     dpda/accepting-configuration?
                     dpda/discard-config?)
   dpda
   "abc"
   )
  
  )
