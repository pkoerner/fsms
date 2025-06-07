(ns fsms.dpda-parser
  (:use [fsms.commons]
        [fsms.regex-tools]
        [fsms.parse-commons])
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))


(defn parse-line [line-str]
  (let [line (s/trim line-str)]
    (cond (s/blank? line) nil
          (s/starts-with? line ";") nil
          (s/starts-with? line "final") (parse-final-states line)
          (s/starts-with? line "start") (parse-start line)
          (s/starts-with? line "(") (parse-pda-transition line)
          :else (assert false (str "PARSE CRITICAL: unexpected input: " line-str)))))

(defn parse-dpda-file [file]
  (keep parse-line (line-seq (io/reader file))))

(defn build-delta [trans]
  (into {} (map (fn [[k vs]] [k (map second vs)]) (group-by first trans))))

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
    (assert (= #{:final-states :start} (set (mapcat keys maps))) "PARSE CRITICAL: expected a declaration of start and final states")
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
