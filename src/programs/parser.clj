(ns programs.parser
  (:require [instaparse.core :as insta]))

(def loop-grammar
  "<Instrs> := Instr <';'> Instrs | Instr
   <Instr> := AssignConstant | AssignCalc | Loop
   AssignCalc := Id <':='> Id Op Number 
   AssignConstant := Id <':='> Number
   Loop := WS <'LOOP'> Id <'DO'> Instrs <'END'> WS
   <Op> := '+' | '-'
   Id := WS 'x' Number WS
   <Number> := WS #'[0-9]+' WS
   <WS> := <#'\\s*'>")

(def loop-parser (insta/parser loop-grammar))

(defn parse-loop-program [s]
  (insta/transform
    {:Id (partial apply str)}
    (loop-parser s)))


(def while-grammar
  "<Instrs> := Instr <';'> Instrs | Instr
   <Instr> := AssignConstant | AssignCalc | Loop | While
   AssignCalc := Id <':='> Id Op Number 
   AssignConstant := Id <':='> Number
   Loop := WS <'LOOP'> Id <'DO'> Instrs <'END'> WS
   While := WS <'WHILE'> Id <'/='> WS <'0'> WS <'DO'> Instrs <'END'> WS
   <Op> := '+' | '-'
   Id := WS 'x' Number WS
   <Number> := WS #'[0-9]+' WS
   <WS> := <#'\\s*'>"
  )

(def while-parser (insta/parser while-grammar))

(defn parse-while-program [s]
  (insta/transform
    {:Id (partial apply str)}
    (while-parser s)))

(def goto-grammar
  "<Program> := Instr+
   Instr := WS Label1? Instr2 WS <';'> WS
   Label1 := Label WS <':'> WS
   Label := #'\\w'+
   <Instr2> := AssignConstant | AssignCalc | Goto | Jump | Halt
   AssignCalc := Id <':='> Id Op Number 
   AssignConstant := Id <':='> Number
   Goto := WS <'GOTO'> WS Label
   Jump := WS <'IF'> Id <'='> WS <'0'> WS <'THEN'> WS <'GOTO'> WS Label WS
   Halt := <'HALT'>
   <Op> := '+' | '-'
   Id := WS 'x' Number WS
   <Number> := WS #'[0-9]+' WS
   <WS> := <#'\\s*'> 
  "
  )

(def goto-parser (insta/parser goto-grammar))

(defn parse-goto-program [s]
  (insta/transform
    {:Id (partial apply str)
     :Label (partial apply str)
     }
    (goto-parser s)))

(defn parse-with [file f]
  (f (slurp file)))

(def primrec-grammar
  "Defs := Def*
   <Def> := Primrec0 | PrimRecInd | Subst
   Subst := Id <'('> Id Comma-Separated-ID-List <')'> WS <'='> WS Expr
   Primrec0 := Id <'('> <'0'> Comma-Separated-ID-List <')'> WS <'='> WS Expr
   PrimRecInd := Id <'('> IDPLUS1 Comma-Separated-ID-List <')'> WS <'='> WS Expr
   Comma-Separated-ID-List := eps | WS <','> Id Comma-Separated-ID-List
   Expr := Id <'('> Comma-Separated-Expr-List <')'> | SimpleExpr
   <Comma-Separated-Expr-List> := Expr | Expr <','> Comma-Separated-Expr-List
   SimpleExpr := Id | Number
   IDPLUS1 := WS Id WS <'+'> WS <'1'> 
   Id := WS #'[a-zA-Z]\\w*' WS
   Number := WS #'[0-9]+' WS
   <WS> := <#'\\s*'> 
  "
  )

(def primrec-parser (insta/parser primrec-grammar))
(defn parse-primrec [s]
  (insta/transform {:Comma-Separated-ID-List (fn ([] [])
                                                 ([a b] (vec (cons a b))))
                    :Id identity
                    :Number parse-long
                    :SimpleExpr identity
                    :Expr (fn [& args] (if (= 1 (count args)) (first args) (vec args)))
                    :Subst (fn [fname id1 idmore expr] [:Subst fname `[~id1 ~@idmore] expr])
                    }
                   (primrec-parser s)))

(def primrec-gödel-grammar
  "<Fn> := Number | Succ | IdFn | Subst | PrimRec
   FnList := Fn | Fn <','> FnList
   Identifier := 'x' Pipes
   IdFn := <'id'> Pipes <'*'> Pipes
   Pipes := '|'+
   <Succ> := 's'
   Number := '0' | <Succ> <'('> Number <')'> 
   Subst := <'SUB'> <'['> Fn <';'> FnList <']'> <'('> IdList <')'>
   PrimRec := <'PR'> <'['> Fn <','> Fn <']'> <'('> IdList <')'>
   IdList := Number | Identifier | Identifier <','> IdList"
  
  )

(def primrec-gödel-parser (insta/parser primrec-gödel-grammar))

(defn parse-primrec-gödel [s]
  (first (insta/transform {:Identifier str
                           :Pipes (comp count str)
                           :IdList (fn ([] []) ([a] [a]) ([a b] (vec (cons a b))))
                           :Number (fn [x] (if (= x "0") 0 (inc x)))
                           :FnList (fn ([] []) ([a] [a]) ([a b] (vec (cons a b))))
                           }
                          (primrec-gödel-parser s))))

(parse-primrec-gödel "PR[id|*|,SUB[s;id|||*||](x|,x||,x|||)](x|,x||)")
[:PrimRec [:IdFn 1 1] [:Subst "s" [[:IdFn 3 2]] ["x1" "x2" "x3"]] ["x1" "x2"]]
(parse-primrec-gödel "SUB[s;s](s(s(s(0))))")
(parse-primrec-gödel "PR[s(0),SUB[PR[0,SUB[PR[id|*|,SUB[s;id|||*||](x|,x||,x|||)](x|,x||);id|||*||,id|||*|||](x|,x||,x|||)](x|,x||);id|||*||,id|||*|||](x|,x||,x|||)](x|,x||)")
(parse-primrec "add(0,x) = id1_1(x)
                add(n+1,x) = s(id3_2(n, add(n,x), x))
                mult(0,x) = 0
                mult(n+1,x) = f2(n, mult(n,x),x)
                f2(a,b,c) = add(id3_2(a,b,c),id3_3(a,b,c))
                f3(a,b,c) = 42")

(comment 
(parse-goto-program
  "M1 : x0 := x1 + 0;
   M2 : IF x2 = 0 THEN GOTO M6;
    x0 := x0 + 1;
   x2 := x2 - 1;
   M5 : GOTO M2;
   M6 : HALT;"
  )

(parse-loop-program 
  "x1 := 43 ; x2 := x42 + 2")

(parse-loop-program 
  "x1 := 43")

(parse-loop-program 
   "x2 := 1; x3 := 1;
   LOOP x1 DO x2 := 0 END;
   LOOP x2 DO x0 := 1; x3 := 0 END ;
   LOOP x3 DO x0 := 0 END")

(parse-loop-program
  "x0 := x1 + 0;
  LOOP x2 DO
  x0 := x0 + 1
  END"
  )

;; this does not parse as a second loop is required
(parse-loop-program
  "x0 := 0;
  LOOP x2 DO
  x0 := x0 + x1
  END"
  )

(parse-while-program 
  "x1 := x2 + 0;
  WHILE x1 /= 0 DO x1 := x1 - 1; x42 := 3 END")

(primrec-parser "args(0) = 42
                 manyargs(0, x,y,z,a,b) = 43
                 add(0,x) = id1_1(x)
                 add(n+1,x) = s(id3_2(n, add(n,x),x))")

(parse-primrec  "args(0) = 42
                 manyargs(0, x,y,z,a,b) = 43
                 add(0,x) = id1_1(x)
                 add(n+1,x) = s(id3_2(n, add(n,x),x))
                 badd(x,y) = foo(s(id2_1(x,y)))")


(parse-primrec "add(0,x) = id1_1(x)
                add(n+1,x) = s(id3_2(n, add(n,x), x))
                mult(0,x) = 0
                mult(n+1,x) = f2(n, mult(n,x),x)
                f2(a,b,c) = add(id3_2(a,b,c),id3_3(a,b,c))
                exp(0,x) = 1
                exp(n+1,x) = f3(n, exp(n,x),x)
                f3(a,b,c) = mult(id3_2(a,b,c),id3_3(a,b,c))
                fa(0) = 1
                fa(n+1) = f4(n, fa(n))
                f4(a,b) = mult(id2_2(a,b), s(id2_1(a,b)))") ;; TODO: is this really allowed?

"fa(0) = 1
 fa(x1+1) = f4(x1,f(x1))
 f4(x1,x2) = f_6530(id2_2(x1,x2),s)
 f_6530(0,x2) = 0
 f_6530(x1+1,x2) = h_6534(x1,f_6530(x1,x2),x2)
 h_6534(x1,x2,x3) = f_6535(id3_2(x1,x2,x3),id3_3(x1,x2,x3))
 f_6535(0,x2) = id1_1(x1,x2)
 f_6535(x1+1,x2) = s"

(parse-primrec-gödel "PR[s(0),SUB[PR[0,SUB[PR[id|*|,s](x|,x||);id|||*||,id|||*|||](x|,x||,x|||)](x|,x||);id||*||,s](x|,x||)](x|)")


)
