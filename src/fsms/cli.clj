(ns fsms.cli
  (:require [clojure.string]
            [clojure.tools.cli :refer [parse-opts]]))

(def valid-cli-options
  "java -jar mytool.jar COMMAND [ARGS*]

Valid options for COMMAND are:
  check-dpda <dpda-file> <config-file>
   | Check an implementation of a DPDA against
   | a number of accepted and rejected words. "
  )

(defn usage [summary]
  (str valid-cli-options \newline \newline 
       "Valid options for ARGS are:" \newline 
       summary))

(def cli-options
  [[nil "--debug" "Be more (or very verbose) in the output."]
   ["-f" "--output FILE" "File to output results into."
    :default nil]
   ["-s" "--score SCORE" "Maximum score for the exercise."
    :default 10
    :parse-fn parse-long]
   ["-h" "--help"] ])


(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
        {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
        {:exit-message (clojure.string/join errors)}
      ;; custom validation on arguments
      (and (= "check-dpda" (first arguments))
           (= 3 (count arguments)))
         {:action (first arguments) :args (rest arguments)
          :options options}
      (and (= "check-tm" (first arguments))
           (= 3 (count arguments)))
         {:action (first arguments) :args (rest arguments)
          :options options}
      :else ; failed custom validation => exit with usage summary
        {:exit-message (usage summary)})))


(defn exit [status msg]
  (println msg)
  (System/exit status))
