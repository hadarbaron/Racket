#lang pl

;;------------Q1----------------
; The ROL BNF and Parsing code:

;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; The actual interpreter
#| BNF for the RegE language:
 <ROL> ::=
      |{ reg-len = len <RegE>}(0)
 <RegE> ::= <Bits>  (1)
      |{shl <RegE>} (2)
      |{and <RegE> <RegE>} (3)
      |{or <RegE> <RegE>} (4)

<Bits> ::= { <Bit> } (5)

<Bit> ::= |1  (6)
          |0  (7)
          |0 <Bit> (8)
          |1 <Bit> (9)

a) "{ reg-len = 3 {1 0 0}} " : <ROL> (0) -> {reg-len = <num> <RegE>} (1) -> {reg-len = <num> <Bits>} (5)(9) ->  {reg-len = <num> {1 <Bit>}}
(8) -> {reg-len = <num> {1 0 <Bits>}} ->  {reg-len = <num> {1 0 0}}

b) "{ reg-len = 3 {shl {1 0 0 }}}" : <ROL> (0) -> { reg-len = <num> <RegE> } (2) -> { reg-len = <num> {shl <RegE>} }
(1) -> { reg-len = <num> {shl <Bits>} } (5)(9) -> { reg-len = <num> {shl {1 <Bit>}} (8)(8) -> { reg-len = <num> {shl {1 0 0} }

c) "{ reg-len = 3 {and {shl {1 0 1 }}{shl {1 0 1 }}}}" <ROL> (0) -> { reg-len = <num> <RegE> } (3) -> { reg-len = 3 {and <RegE> <RegE>} }
 (2) -> { reg-len = 3 {and {shl <RegE>} {shl <RegE>} } (1) -> { reg-len = 3 {and {shl <Bits>} {shl <Bits>} } (9)(8)(6) -> { reg-len = 3 {and {shl {1 0 1} {shl {1 0 1}} } 

|#

;; RegE abstract syntax trees
(define-type RegE
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Shl RegE])

;; Next is a technical function that converts (casts)

;; (any) list into a bit-list. We use it in parse-sexpr.
 (: list->bit-list : (Listof Any) -> Bit-List)
 ;; to cast a list of bits as a bit-list
 (define (list->bit-list lst)
 (cond [(null? lst) null]
 [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
 [else (cons 0 (list->bit-list (rest lst)))]))

;;The function checks if the arriving syntax is in form  {reg-len = +num RegE}, If not, returns an error.
(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
 (define (parse-sexpr sexpr)
 (match sexpr
   [(list 'reg-len '= (number: n) r) (if (> n 0) (parse-sexpr-RegL r n) (error 'parse-sexpr "bad Length ~s" n))] ;; remember to make sure specified register length is at least 1
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
 (: parse-sexpr-RegL : Sexpr Number -> RegE)


;The function checks if the arriving syntax is in form  {or/and/shl/bits}, If not, returns an error.
;; to convert s-expressions into RegEs
 (define (parse-sexpr-RegL sexpr reg-len)
 (match sexpr
 [(list (and a (or 1 0)) ... ) (if(equal? (length a) reg-len) (Reg(list->bit-list a))
 (error 'parse-sexpr "wrong number of bits in ~s" a))]
 [(list 'and l r) (And (parse-sexpr-RegL l reg-len) (parse-sexpr-RegL r reg-len))]
 [(list 'or l r) (Or (parse-sexpr-RegL l reg-len) (parse-sexpr-RegL r reg-len))]
 [(list 'shl a) (Shl (parse-sexpr-RegL a reg-len))]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
 (: parse : String -> RegE)

;; parses a string containing a RegE expression to a RegE AST
 (define (parse str)
 (parse-sexpr (string->sexpr str)))

;; tests
 (test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
 (test (parse "hadar") =error> "bad syntax in hadar" )
 (test (parse "{ reg-len = -4 {1 0 0 0}}") =error> "bad Length -4" )
 (test (parse "{ reg-len = 4 hadar}") =error> "bad syntax in hadar" )
 (test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
 (test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
 (test (parse "{ reg-len = 4 {or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
 (test (parse "{ reg-len = 2 {or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
 (test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")

 ;;-----------------Q2------------------------
#|
1) The problem is, there can be several different types of shear trees for the same phrase that don't bring the same end result.



<MAE> = {seq <SET> <SET> <GET>}
        {seq <AE>}
        {seq <AE> <AE>}
 
<SET> = {set <AE>}
        {set <GET>}
        
<GET> = get
        <NUM>
       | {+ <GET> <GET>} 
       | {- <GET> <GET>} 
       | {* <GET> <GET>} 
       | {/ <GET> <GET>}

<AE> ::= <num> 
       | {+ <AE> <AE>} 
       | {/ <AE> <AE>} 
       | {* <AE> <AE>} 
       | {- <AE> <AE>}


input: 31612884
1)<MAE> 

-> {seq <SET> <SET> <GET>} 

-> {seq {set <AE>} {set <GET>} {<GET>}

-> {seq {set {+ <NUM> <NUM>}} {set {* <GET> <GET>} {/ <GET> <GET>}

-> {seq {set {+ 31 612}} {set {* get get} {/ get <NUM>}

-> {seq {set {+ 31 612}} {set {* get get} {/ get 2}


2)<MAE> 

-> {seq <SET> <SET> <GET>}

-> {seq {set <AE>} {set <GET>} {<GET>}

-> {seq {set {+ <NUM> <AE>} {set {* <GET> <GET>} {/ <GET> <GET>}

-> {seq {set {+ 8 {* <AE> <AE>}} {set {* {*<GET> <GET>} {* <GET> <GET>}} {/ get <NUM>}

-> {seq {set {+ 8 {* <NUM> <NUM>}} {set {* {* get get} {* get get}} {/ get 2}

-> {seq {set {+ 8 {* 28 884}} {set {* {* get get} {* get get}} {/ get 2}

 
3)<MAE>

{seq <AE>} 

{seq {- <AE> <AE>}}

{seq {- <NUM> <NUM>}}

{seq {- 16 12}}


|#

;;------------Q3---------------
 
;The function gets a list and returns the sum of the squared organs by using map foldl and internal function 
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map (lambda ([n : Number])(* n n)) lst)))

(test (sum-of-squares (list 1 1 1)) => 3)
(test (sum-of-squares (list 2 2 2)) => 12)
(test (sum-of-squares (list 1 2 3)) => 14)

;;------------Q4---------------

;Binary tree builder
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])


(: add1 : Number -> Number)
(define (add1 number)(+ number 1))

;A function that receives a tree and a function and executes it on every leaf in the tree
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map function tree)
  (cases tree
    [(Node r l) (Node (tree-map function r) (tree-map function l))]
    [(Leaf num) (Leaf (function num))]
    )
)

;The function accepts two functions. One that operates on a single organ. And one that operates on two organs.
;It also receives a tree or whatever and individually activates the function on a single organ. And between every two organs the function of two organs is activated
(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold function leaff tree)
  (cases tree
    [(Node r l) (function (tree-fold function leaff r) (tree-fold function leaff l))]
    [(Leaf num) (leaff num)]
    )
)

;The function transforms the tree by using the function tree-fold
(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse tree)
  (cases tree
   [(Node r l)
    (Node (tree-reverse (tree-fold Node leaf_function l))
          (tree-reverse (tree-fold Node leaf_function r)))]
   [(Leaf num) (Leaf num)]))

( : leaf_function : Number -> BINTREE )
(define (leaf_function n)
 ( Leaf n))

(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
 (tree-fold (inst append Number) (inst list Number)
tree))

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2)
(Leaf 3))))
 => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-flatten (Node (Leaf 8) (Node (Leaf 9)(Leaf 10))))=> '(8 9 10) )
(test (tree-reverse (Node (Leaf 1) (Leaf 2))) => (Node (Leaf 2) (Leaf 1)))
