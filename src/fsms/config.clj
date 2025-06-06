(ns fsms.config
  (:require [clojure.edn :as edn]))

(defn load-config [file]
  (edn/read-string (slurp file)))
