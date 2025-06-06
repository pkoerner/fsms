(ns fsms.turing-parser
  (:use [fsms.regex-tools]
        [fsms.parse-commons]
        [clojure.java.io :as io])
  (:require [clojure.string :as s]))

(defn parse-transition 
  "attempts parsing a transition form a line such as
      (z0, a) -> (z1, A, R)"
  [line]
  (let [[_ state-from sym state-to new-sym direction :as match]
          (re-find (regex-concat turing-lhs arrow turing-rhs end-of-line)
                   line)]
    (assert match (str "PARSE CRITICAL: not a valid transition: " line))
    (assert (= 1 (count sym)) (str "PARSE CRITICAL: input symbol too long: " sym " in: " line))
    (assert (= 1 (count new-sym)) (str "PARSE CRITICAL: new symbol too long: " new-sym " in: " line))
    [{:state state-from, :symbol sym}
     {:state state-to :symbol new-sym :direction direction}]))


(defn parse-line [line-str]
  (let [line (s/trim line-str)]
    (cond (s/blank? line) nil
          (s/starts-with? line ";") nil
          (s/starts-with? line "final") (parse-final-states line)
          (s/starts-with? line "start") (parse-start line)
          (s/starts-with? line "(") (parse-transition line)
          :else (assert false (str "PARSE CRITICAL: unexpected input: " line-str)))))

(defn parse-tm-file [file]
  (keep parse-line (line-seq (io/reader file))))

(defn build-delta [trans]
  (into {} (map (fn [[k vs]] [k (map second vs)]) (group-by first trans))))

(defn build-tm [infos]
  (let [maps (filter map? infos)]
    (assert (not (<= (count maps) 1)) "PARSE CRITICAL: expected a declaration of start and final states")
    (assert (not (<= 3 (count maps))) "PARSE CRITICAL: too many declaration of start or final states")
    (assert (= #{:final-states :start} (set (mapcat keys maps))) "PARSE CRITICAL: expected a declaration of start and final states")
    (let [maps (filter map? infos)
          base (apply merge maps)
          delta-trans (remove map? infos)
          delta (build-delta delta-trans)]
      (assoc base :delta delta))))


(def file->tm (comp build-tm parse-tm-file))
