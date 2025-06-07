(ns fsms.pda
  (:use [fsms.commons]))

(def MAX-STACKSIZE 30)

(defn initial-configurations [pda word]
  [{:state (get pda :start)
    :input word
    :stack "#"}])


(defn trans [config sym next-state]
  (when next-state
    (-> config
        (assoc :state (:state next-state))
        (update :input (if (= sym lambda) identity (comp (partial apply str) rest)))
        ;; NOTE: An empty / lambda stack is represented as empty string (as returned by the parser). 
        (update :stack (fn [stack] (apply str (concat (:new-stack next-state) (rest stack))))))))

(defn next-states [pda config]
  (if (empty? (:stack config))
    nil
    (let [sym (str (first (get config :input)))
          consume-sym (get-in pda [:delta {:state (get config :state)
                                                   :symbol sym
                                                   :top-of-stack (str (first (get config :stack)))}])
          lambda-trans (get-in pda [:delta {:state (get config :state)
                                                    :symbol lambda
                                                    :top-of-stack (str (first (get config :stack)))}])]
      (concat (map (partial trans config sym) consume-sym)
              (map (partial trans config lambda) lambda-trans)))))

(defn dpda-accepting-configuration? [dpda config]
  (and (empty? (:input config))
       (contains? (get dpda :final-states) (get config :state))))

(defn discard-config? [config]
  (< MAX-STACKSIZE (count (get config :stack))))
