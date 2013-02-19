#lang plai-typed

(require "python-core-syntax.rkt")
(require "python-helper.rkt")

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

(require (typed-in racket/base [display : (string -> void)]))

(define (pretty [arg : CVal]) : string
  (type-case CVal  arg
    [VNum (n) (to-string n)]
    [VStr (s) (constr s)]
    [VClosure (env args body mems) (error 'prim "Can't print closures yet")]
    [VBool (b) 
           (if b "true" "false")]
    [VNone() ""]
    [VDict(d) "a dict"]
    [VMembers(t m) "a object with members"]
    
    ))

(define (pretty-obj [arg : VObjType]) : string
  (cond 
    [(symbol=? (VObj-type arg) 'int) 
     (constr ( list  (VObj-id arg) 
                     (symbol->string (VObj-type arg))  
                     (pretty (VObj-value arg))))]
    
    [(symbol=? (VObj-type arg) 'str) 
     (constr ( list  (VObj-id arg) 
                     (symbol->string (VObj-type arg))  
                     (pretty (VObj-value arg))))]
    
    
    
    [(symbol=? (VObj-type arg) 'bool) 
     (constr ( list  (VObj-id arg) 
                     (symbol->string (VObj-type arg))  
                     (pretty (VObj-value arg))))]
    
    [(symbol=? (VObj-type arg) 'NoneType) 
     "None\n"]
    
    ))

(define (print [arg : VObjType]) : void
  (display (pretty-obj arg)))

(define (python-prim1 [op : symbol] [arg : VObjType]) : VAnswer
  (case op
    [(print) (begin (print arg) (VResult arg))]))

