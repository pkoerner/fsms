(ns fsms.dpda-parser
  (:use [fsms.commons])
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn parse-final-states [line]
  (let [[final & fstates] (re-seq #"\S+" line)]
    (if (seq fstates)
      {:final-states (set fstates)}
      (assert false (str "PARSE CRITICAL: No accepting states found in line: " line)))))

(defn parse-transition [line]
  (let [[_ state-from sym tos state-to new-stack :as match]
          (re-find #"\(\s*(\w+)\s*,\s*(\w+)\s*,\s*(\S+)\s*\)\s*->\s*\(\s*(\w+)\s*,\s*(\S+)\s*\)\s*$" 
                   line)]
    (assert match (str "PARSE CRITICAL: not a valid transition: " line))
    (assert (= 1 (count sym)) (str "PARSE CRITICAL: input symbol too long: " sym " in: " line))
    (assert (not= tos lambda) (str "PARSE CRITICAL: lambda not allowed as top of stack in: " line))
    [{:state state-from, :symbol sym :top-of-stack tos}
     {:state state-to :new-stack (s/replace new-stack "_" "")}]))

(defn parse-start [line]
  (let [[_ start] (re-find #"^start\s+(\w+)\s*$" line)]
    (assert start
            (str "no valid start state found in line: " line))
    {:start start}))


(defn parse-line [line-str]
  (let [line (s/trim line-str)]
    (cond (s/blank? line) nil
          (s/starts-with? line "final") (parse-final-states line)
          (s/starts-with? line "start") (parse-start line)
          (s/starts-with? line "(") (parse-transition line)
          :else (assert false (str "PARSE CRITICAL: unexpected input: " line-str)))))

(defn parse-dpda-file [file]
  (keep parse-line (line-seq (io/reader file))))

(defn build-delta [trans]
  (into {} trans))

(defn validate-deterministic [delta-trans delta]
  (assert (= (count delta-trans) (count delta)) "CRITICAL: transition function is not deterministic")
  (doseq [[{:keys [state symbol top-of-stack]} _rhs] delta-trans
        :when (not= symbol lambda)]
    (assert (not (contains? delta {:state state, :symbol lambda, :top-of-stack top-of-stack}))
            (str "CRITICAL: transition function is not deterministic, as " [state symbol top-of-stack] " as well as " [state lambda top-of-stack] " is defined"))))

(defn build-dpda [infos]
  (let [maps (filter map? infos)]
    (assert (not (<= (count maps) 1)) "PARSE CRITICAL: expected a declaration of start and final states")
    (assert (not (<= 3 (count maps))) "PARSE CRITICAL: too many declaration of start or final states")
    (let [maps (filter map? infos)
          base (apply merge maps)
          delta-trans (remove map? infos)
          delta (build-delta delta-trans)]
      (validate-deterministic delta-trans delta)
      (assoc base :delta delta))))

(def file->dpda (comp build-dpda parse-dpda-file))

(comment
(file->dpda (io/resource "dpda.edn"))
(parse-dpda-file (io/resource "dpda.edn")))
