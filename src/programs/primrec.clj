(ns programs.primrec
  (:require [clojure.string :as string]))

(def defs [:Defs #_[:Primrec0 "args" [] 42]
                 #_[:PrimRecInd "args" [:IDPLUS1 "n"] 42]
                 [:Primrec0 "add" ["x"] ["id1_1" "x"]]
                 [:PrimRecInd "add" [:IDPLUS1 "n"] ["x"] ["s" ["id3_2" "n" ["add" "n" "x"] "x"]]]
                 [:Subst "badd" ["x" "y"] ["foo" ["s" ["id2_1" "x" "y"]]]]] )

(defn get-fname [definition]
  (second definition))

(defn get-args [definition]
  (case (first definition)
    :Primrec0 (nth definition 2)
    :PrimRecInd (cons (second (nth definition 2)) (nth definition 3))
    :Subst (nth definition 2)))

(defn enforce-arg-pattern [expr pattern]
  (cond
    (number? expr) true
    (sequential? expr)
    (let [[f & args] expr]
      (or (= args pattern)
          (apply = true (map #(enforce-arg-pattern % pattern) args))))))

(enforce-arg-pattern ["s" ["id3_2" "n" ["add" "n" "x"] "x"]]
                     ["n" ["add" "n" "x"] "x"])

(enforce-arg-pattern ["id1_1" "x"] ["x"])

(defn verify-primrec0 [definitions [_primrec0 fname args expr]]
  (cond (not (or (number? expr)
                 (enforce-arg-pattern expr args)))
        (println (str "Invalid definition of " fname \( (string/join "," (cons 0 args)) \) ": Not a constant and wrong number of arguments passed."))
        (not (some (fn [[typey fnamey _ argsy]] (and (= typey :PrimRecInd) (= fnamey fname) (= (count args) (count argsy)))) definitions))
        (println (str "missing recursive definition of " fname "(n+1, ...) with proper arity"))
        :else true))

(defn verify-primrec-ind [definitions [_primrecind fname [_idplus1 recvar] args expr]]
  (cond (not (enforce-arg-pattern expr `[~recvar [~fname ~recvar ~@args] ~@args]))
        (println (str "Invalid definition of " fname \( (string/join "," (cons (str recvar "+1")  args)) \) ": Wrong number of arguments passed."))
        (not (some (fn [[typey fnamey argsy]] (and (= typey :Primrec0) (= fnamey fname) (= (count args) (count argsy)))) definitions))
        (println (str "missing recursive definition of " fname "(n+1, ...) with proper arity"))
        :else true))

(defn verify-subst [definitions [_subst fname args expr]]
  (if (enforce-arg-pattern expr args)
    true
    (println (str "Invalid definition of " fname ": wrong arguments schema passed"))))

(defn verify-definition [definitions defy]
  (case (first defy)
    :Primrec0 (verify-primrec0 definitions defy)
    :PrimRecInd (verify-primrec-ind definitions defy)
    :Subst (verify-subst definitions defy)))

(defn remove-known [fns]
  (remove (fn [f] (or (= "s" f)
                      (clojure.string/starts-with? f "id")))
          fns))

(defn find-uses [definition]
  (let [skip? (if (= (first definition) :PrimRecInd)
                (let [[_primrecind fname [_idplus1 recvar] args _expr] definition]
                  (fn [expr] (= (rest expr) `[~recvar [~fname ~recvar ~@args] ~@args])))
                (constantly false))]
    (loop [exprs (filter sequential? [(last definition)])
           acc #{}]
      (if (empty? exprs)
        [(get-fname definition) acc]
        (recur (remove (complement sequential?) (mapcat rest (remove skip? exprs)))
               (into acc (remove-known (map first exprs))))))))

(find-uses [:PrimRecInd "add" [:IDPLUS1 "n"] ["x"] ["s" ["id3_2" "n" ["add" "n" "x"] "x"]]])

(defn no-cyclic-definitions [definitions]
  (let [uses (map find-uses definitions)]
    (loop [unresolved uses]
      (if (empty? unresolved)
        true
        (let [cands (filter (comp empty? second) unresolved)]
          (if (seq cands)
            (recur (map (fn [[x y]] [x (remove #{(ffirst cands)} y)]) (remove #{(first cands)} unresolved)))
            (println (str "Functions might be part of a cyclic definition or undefined: " (string/join " " (concat (map first unresolved) (mapcat second unresolved)))))))))))

(defn get-arity [definition]
  (case (first definition)
    :Primrec0 (inc (count (nth definition 2)))
    :PrimRecInd (inc (count (nth definition 3)))
    :Subst (count (nth definition 2))))

(defn valid-id? [s]
  (seq (map parse-long (rest (re-find #"^id(\d)_(\d)$" s)))))

(defn id-m_k->m-k [s] (valid-id? s))

(defn ensure-defined-arities [defined-arities expr]
  (cond (number? expr) true
        (string? expr) true
        (sequential? expr)
        (let [fname (first expr)]
          (and (or (= fname "s")
                   (and (string/starts-with? fname "id") (= (count (rest expr)) (first (id-m_k->m-k fname))))
                   (get-in defined-arities [fname (count (rest expr))])
                   (println (str "arity " (count (rest expr)) " of function " fname " undefined")))
               (every? true? (map (partial ensure-defined-arities defined-arities) (rest expr)))))))


;; TODO: do not allow re-definition

(defn correct-arities [definitions]
  (let [defined (map (fn [x] [(get-fname x) (get-arity x)]) definitions)
        defined-arities (into {} (map (fn [[fname tuples]] [fname (into #{} (map second tuples))]) (group-by first defined)))]
    (every? true? (map (partial ensure-defined-arities defined-arities) (map last definitions)))))

(defn no-reserved-identifiers-defined [definitions]
  (let [illegal (filter (fn [x] (or (= x "s") (string/starts-with? x "id"))) (map get-fname definitions))]
    (if (seq illegal)
      (println (str "illegal identifier(s) defined: " (string/join " " illegal)))
      true)))

(defn gather-vars [expr]
  (cond (number? expr) []
        (string? expr) [expr]
        (sequential? expr) (mapcat gather-vars (rest expr))))

;; NOTE: this probably does not anything extra compared to enforcing the argument patterns
(defn ensure-defined-variables [definition]
  (let [args (get-args definition)]
    (cond (not= (count (set args)) (count args))
            (println (str "Duplicate argument in " (get-fname definition) "(" (string/join "," args) ")"))
          (not= #{} (clojure.set/difference (set (gather-vars (last definition))) (set args)) )
            (println (str "undefined variable(s) " (string/join "," (clojure.set/difference (set (gather-vars (last definition))) (set args))) 
                          " in " (get-fname definition)))
          :otherwise true)))

(defn no-redefinition [definitions]
  (let [defined (map second (remove (comp #{:PrimRecInd} first) (map (fn [definition] [(first definition) (get-fname definition)]) definitions)))
        freqs (frequencies defined)
        dupes (filter (fn [[k n]] (< 1 n)) freqs)]
    (if (seq dupes)
      (println (str "duplicate definition of function(s): " (string/join ", " (map first dupes))))
      true)))

(defn verify-definitions [definitions]
  (let [actualdefs (rest definitions)]
    (and (every? true? (map (partial verify-definition actualdefs) actualdefs))
         (no-cyclic-definitions actualdefs)
         (correct-arities actualdefs)
         (no-reserved-identifiers-defined actualdefs)
         (every? true? (map ensure-defined-variables actualdefs))
         (no-redefinition actualdefs)
         )))

(verify-definitions defs)
(verify-definitions 
[:Defs [:Primrec0 "add" ["x"] ["id1_1" "x"]]
       [:PrimRecInd "add" [:IDPLUS1 "n"] ["x"] ["s" ["id3_2" "n" ["add" "n" "x"] "x"]]]
       [:Primrec0 "mult" ["x"] 0]
       [:PrimRecInd "mult" [:IDPLUS1 "n"] ["x"] ["f2" "n" ["mult" "n" "x"] "x"]]
       [:Subst "f2" ["a" "b" "c"] ["add" ["id3_2" "a" "b" "c"] ["id3_3" "a" "b" "c"]]]
       [:Subst "f3" ["a" "b" "c"] 42]
        ])


(map get-arity (rest defs))
(no-cyclic-definitions (rest defs))
(find-uses (first (rest defs)))
(correct-arities (rest defs))

(verify-primrec0 (rest defs) (nth defs 1))


(defn pretty-print-expr [expr]
  (cond (number? expr) (str expr)
        (string? expr) expr
        (sequential? expr) (str (first expr) "(" (string/join "," (map pretty-print-expr (rest expr))) ")")))

(defn pretty-print-primrec0 [[_primrec0 fname args expr]]
  (str fname "(" (clojure.string/join "," (cons 0 args)) ") = " (pretty-print-expr expr)))
(defn pretty-print-primrecind [[_primrecind fname [_idplus1 recvar] args expr]]
  (str fname "(" (clojure.string/join "," (cons (str recvar "+1") args)) ") = " (pretty-print-expr expr)))
(defn pretty-print-subst [[_subst fname args expr]]
  (str fname "(" (clojure.string/join "," args) ") = " (pretty-print-expr expr)))

(defn pretty-print-definition [definition]
  (case (first definition)
    :Primrec0 (pretty-print-primrec0 definition)
    :PrimRecInd (pretty-print-primrecind definition)
    :Subst (pretty-print-subst definition)))

(defn pretty-print-definitions [defs]
  (string/join "\n" (map pretty-print-definition (rest defs))))

(println (pretty-print-definitions defs))

(def defs [:Defs #_[:Primrec0 "args" [] 42]
                 #_[:PrimRecInd "args" [:IDPLUS1 "n"] 42]
                 [:Primrec0 "add" ["x"] ["id1_1" "x"]]
                 [:PrimRecInd "add" [:IDPLUS1 "n"] ["x"] ["s" ["id3_2" "n" ["add" "n" "x"] "x"]]]
                 [:Subst "badd" ["x" "y"] ["foo" ["s" ["id2_1" "x" "y"]]]]] )

;; TODO: do not generate a function for numbers
(defn expansion-rules [p args default]
  (cond (number? p) p
        (= "s" p) p
        (and (sequential? p) (= (first p) :IdFn)) `[~(str "id" (nth p 1) "_" (nth p 2)) ~@args]
        :else default))

(defn ungödel-aux [namey p]
  (cond (= p "s") [#_[:Subst namey ["x"] ["s" "x"]]]
        (number? p) []
        :else
        (case (first p)
          :IdFn (let [[_ m k] p
                      args (mapv (fn [n] (str "x" (inc n))) (range m))
                      ]
                  [#_[:Subst namey args `[~(str "id" m "_" k) ~@args]]]
                  )
          :PrimRec (let [[_ BaseCase RecurCase args] p
                         g-str (name (gensym "g_"))
                         h-str (name (gensym "h_"))]
                     (concat [[:Primrec0 namey (vec (rest args)) (expansion-rules BaseCase args `[~g-str ~@(rest args)])]
                              [:PrimRecInd namey [:IDPLUS1 (first args)] (vec (rest args)) (expansion-rules RecurCase args `[~h-str ~(first args) [~namey ~@args] ~@(rest args)])]]
                             (ungödel-aux g-str BaseCase)
                             (ungödel-aux h-str RecurCase)))
          :Subst (let [[_ f gs args] p
                       f-str (name (gensym "f_"))
                       g-strs (map (fn [_] (name (gensym "g_"))) gs)]
                   (concat [ [:Subst namey args `[~(cond (= f "s") "s" (and (sequential? f) (= (first f) :IdFn)) (str "id" (nth f 1) "_" (nth f 2)) :else f-str) ~@(map (fn [g-str g] (expansion-rules g args `[~g-str ~@args])) g-strs gs)]]]
                           (ungödel-aux f-str f)
                           (mapcat (fn [g-str g] (ungödel-aux g-str g)) g-strs gs)))
          ))
  )

(defn ungödel [p]
  (vec (cons :Defs (ungödel-aux "f" p)))
  
  )

(ungödel [:PrimRec 1 [:IdFn 2 2] ["x1" "x2"]])
(println (pretty-print-definitions (ungödel [:PrimRec [:IdFn 1 1] [:Subst "s" [[:IdFn 3 2]] ["x1" "x2" "x3"]] ["x1" "x2"]])))
(println (pretty-print-definitions (ungödel [:PrimRec 1 [:Subst [:PrimRec 0 [:Subst [:PrimRec [:IdFn 1 1] [:Subst "s" [[:IdFn 3 2]] ["x1" "x2" "x3"]] ["x1" "x2"]] [[:IdFn 3 2] [:IdFn 3 3]] ["x1" "x2" "x3"]] ["x1" "x2"]] [[:IdFn 3 2] [:IdFn 3 3]] ["x1" "x2" "x3"]] ["x1" "x2"]])))

(defn gödelify-number [n]
  (str (apply str (repeat n "s("))
       0
       (apply str (repeat n ")"))))

(defn gödelify-id [fname]
  (let [[m k] (id-m_k->m-k fname)]
    (str "id" (apply str (repeat m "|")) "*" (apply str (repeat k "|")))))

(defn gödelify-arg [n]
  (apply str "x" (repeat n "|")))


(defn gödelify-aux [defs fname]
  (let [cands (filter (fn [x] (= fname (get-fname x))) defs)]
    (case (count cands)
      0 ;; must be number or id or something
      (cond (number? fname) (gödelify-number fname)
            ;(= fname "s") "s"
            (string/starts-with? fname "id") (gödelify-id fname)
            :else (println :unhandled fname))
      1 ;; must be substitution
      (let [[_subst _fname argv [f & gs]] (first cands)]
        (str "SUB[" (gödelify-aux defs f) ";" (string/join "," (map (partial gödelify-aux defs) (map first gs))) "]"
             "(" (string/join "," (map gödelify-arg (range 1 (inc (count argv))))) ")"))
      2 ;; must be primrec
      (let [[[_primrec0 _fname0 args0 expr1] 
             [_primrecind _fname1 [_idplus1 _recvar] _args1 expr2]]
            (if (= (ffirst cands) :Primrec0) cands (reverse cands))
            g (if (sequential? expr1) (first expr1) expr1)
            h (if (sequential? expr2) (first expr2) expr2)
            ]
        (str "PR[" (gödelify-aux defs g) "," (gödelify-aux defs h) "]"
             "(" (string/join "," (map gödelify-arg (range 1 (inc (inc (count args0)))))) ")")))))

(defn gödelify [defs fname]
  (gödelify-aux (rest defs) fname))


(gödelify [:Defs [:Primrec0 "add" ["x"] ["id1_1" "x"]]
                 [:PrimRecInd "add" [:IDPLUS1 "n"] ["x"] ["s" ["id3_2" "n" ["add" "n" "x"] "x"]]]
                 [:Primrec0 "mult" ["x"] 0]
                 [:PrimRecInd "mult" [:IDPLUS1 "n"] ["x"] ["f2" "n" ["mult" "n" "x"] "x"]]
                 [:Subst "f2" ["a" "b" "c"] ["add" ["id3_2" "a" "b" "c"] ["id3_3" "a" "b" "c"]]]
                 [:Primrec0 "exp" ["x"] 1]
                 [:PrimRecInd "exp" [:IDPLUS1 "n"] ["x"] ["f3" "n" ["exp" "n" "x"] "x"]]
                 [:Subst "f3" ["a" "b" "c"] ["mult" ["id3_2" "a" "b" "c"] ["id3_3" "a" "b" "c"]]]
                 [:Primrec0 "fa" [] 1]
                 [:PrimRecInd "fa" [:IDPLUS1 "n"] [] ["f4" "n" ["fa" "n"]]]
                 [:Subst "f4" ["a" "b"] ["mult" ["id2_2" "a" "b"] ["s" ["id2_1" "a" "b"]]]]]
          "fa"
          )

(println (pretty-print-definitions (ungödel [:PrimRec 1 [:Subst [:PrimRec 0 [:Subst [:PrimRec [:IdFn 1 1] "s" ["x1" "x2"]] [[:IdFn 3 2] [:IdFn 3 3]] ["x1" "x2" "x3"]] ["x1" "x2"]] [[:IdFn 2 2] "s"] ["x1" "x2"]] ["x1"]])))
