#lang plai-typed
(require "python-core-syntax.rkt" )

(define  (printCExp [expr : CExp])
  (type-case CExp expr
    [CNum (n) (display n)]
    [CStr (s) (display s)]
    [CBool(b)  (display b)]
    [CSeq (e1 e2 ) (display "seq") ]
    [CError (e1)  (display "error")]
    [CIf (test then elsecase) (display "if")]
    [CId (x) (display (symbol->string x))]
    [CLet (x bind body) (display "let")]
    [CApp (fun args ) (display "App")]
    [CLambda (args body) (display "fun")]
    [CPrim1 (prim arg)  (display "cprim1")]
    [CRaise (e) (display "raise yo")]
    [CPass() (display "lazy nothing")]
    [CBoolOp(op l r ) (display "bool op")]
    [CUnaryOp(op operand) (display "unary op")]
    [else (display "not immted")]
    ))


;concatenate strs
(define (constr [strs : (listof string)])
  (cond
    [(empty? strs) ""]
    [else
     (string-append (first strs) (constr (rest strs)))]))