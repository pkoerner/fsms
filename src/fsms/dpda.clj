(ns fsms.dpda
  (:use [fsms.commons]))

(def MAX-STACKSIZE 30)

(defn initial-configurations [dpda word]
  [{:state (get dpda :start)
    :input word
    :stack "#"}])


(defn trans [config sym next-state]
  (when next-state
    (-> config
        (assoc :state (:state next-state))
        (update :input (if (= sym lambda) identity (comp (partial apply str) rest)))
        ;; NOTE: An empty / lambda stack is represented as empty string (as returned by the parser). 
        (update :stack (fn [stack] (apply str (concat (:new-stack next-state) (rest stack))))))))

(defn next-states [dpda config]
  (if (empty? (:stack config))
    nil
    (let [sym (str (first (get config :input)))
          consume-sym (get-in dpda [:delta {:state (get config :state)
                                            :symbol sym
                                            :top-of-stack (str (first (get config :stack)))}])
          lambda-trans (get-in dpda [:delta {:state (get config :state)
                                             :symbol lambda
                                             :top-of-stack (str (first (get config :stack)))}])]
      (cond consume-sym [(trans config sym consume-sym)]
            lambda-trans [(trans config lambda lambda-trans)]
            :else nil))))

(defn accepting-configuration? [dpda config]
  (and (empty? (:input config))
       (contains? (get dpda :final-states) (get config :state))))

(defn discard-config? [config]
  (< MAX-STACKSIZE (count (get config :stack))))
