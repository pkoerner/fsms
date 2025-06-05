(ns fsms.core
  (:require [fsms.dpda-parser :as dpda-parser]
            [fsms.dpda :as dpda]
            [fsms.search :refer [build-accept?-fn]]
            [clojure.java.io :as io])
  (:gen-class))


(comment
  (def dpda (dpda-parser/file->dpda (io/resource "dpda2.edn")))
  (dpda/initial-configurations dpda "abc")
  (dpda/next-states dpda (first (dpda/next-states dpda  (first (dpda/initial-configurations dpda "abc")))))

  ((build-accept?-fn dpda/initial-configurations
                    dpda/next-states
                    dpda/accepting-configuration?
                    dpda/discard-config?
                    )
   dpda
   "abc"
   )
  
  )
