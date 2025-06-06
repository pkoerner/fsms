(ns fsms.regex-tools)

(def lpar #"\(")
(def rpar #"\)")
(def comma #",")
(def word-chars #"\s*(\w+)\s*")
(def state word-chars)
(def syms word-chars)
(def gamma-syms #"\s*(\S+)\s*")
(def arrow #"\s*->\s*")
(def direction #"\s*(L|N|R)\s*")
(def end-of-line #"\s*$")

(defn regex-concat [& args]
  (re-pattern (apply str args)))

(def state+sym (regex-concat lpar state comma syms rpar))

;; a tuple of the form (z0, a, #)
(def state+sym+gamma (regex-concat lpar state comma syms comma gamma-syms rpar))
(def state+gamma (regex-concat lpar state comma gamma-syms rpar))
(def state+gamma+direction (regex-concat lpar state comma gamma-syms comma direction rpar))

(def pda-lhs state+sym+gamma)
(def pda-rhs state+gamma)
(def turing-lhs state+gamma)
(def turing-rhs state+gamma+direction)

