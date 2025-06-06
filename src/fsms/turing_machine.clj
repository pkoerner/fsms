(ns fsms.turing-machine
  (:use [fsms.commons]) )

(def MAX-BAND-LENGTH 30)

(def blank lambda)

(defn initial-configurations [tm word]
  [{:state (get tm :start)
    :band-left blank
    :current (str (first word))
    :band-right (apply str (rest word))}])

(defn turing-step
  "Executes a single step of a turing machine."
  [tm config] 
  (let [delta (:delta tm)
        transs (get delta {:state (get config :state) :symbol (get config :current)})]
    (for [{:keys [direction symbol] :as trans} transs
          :let [{:keys [state band-left current band-right]} config]] 
      {:state (get trans :state)
       :band-left (case direction
                    "L" (let [b' (apply str (butlast band-left))] (if (empty? b') blank b'))
                    "N" band-left
                    "R" (str band-left symbol)
                    )
       :current (case direction
                  "L" (str (last band-left))
                  "N" (get trans :symbol) 
                  "R" (str (first band-right)))
       :band-right (case direction
                     "L" (apply str symbol band-right)
                     "N" band-right
                     "R" (let [b' (apply str (rest band-right))] (if (empty? b') blank b')))})))

(defn turing-accepting?
  [tm config]
  (contains? (get tm :final-states) (get config :state)))

(defn turing-discard? [config]
  (< MAX-BAND-LENGTH (+ (count (get config :band-left))
                        1
                        (count (get config :band-right)))))
