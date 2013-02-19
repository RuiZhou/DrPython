; library for predefined things


#lang plai-typed

(require "python-core-syntax.rkt")
(require "python-helper.rkt")

 
;; make a resucrisve CSeqs
;;
(define (CSeqs  [binds  : (listof ClassFunc)]) : CExp
  (CSeqs-with-base (rest binds)
                   (CSet!(CId (cf-n  (first binds)))  (cf-e (first binds))))) 


;; make a CSeqs with a particular base
;;
(define (CSeqs-with-base  [binds  : (listof ClassFunc)] [base : CExp] ) : CExp
  (cond 
    [(equal? (length binds) 0) base]
    [else
     (CSeqs-with-base (rest binds)
                      (CSeq (CSet! (CId (cf-n (first binds))) (cf-e (first  binds))) base))]))


;; symbol and function pair
;;
(define-type ClassFunc
  (cf (n : symbol) (e : CExp)))
(define-type-alias Lib (CExp -> CExp))




;; is this callable?
;;
(define callable-func
  (CLambda (list 'x)
           (CCompare 'eq (CPrim1 'tagof  (CId 'x)) (CStr (string->list "func")))))


;; print function
;;
(define print-lambda
  (CLambda (list 'to-print)
           (CPrim1 'print (CId 'to-print))))


;; assert function : true
;;
(define assert-true-lambda
  (CLambda (list 'check-true)
           (CIf (CId 'check-true) (CBool #t) (CError 'Exception (CStr (string->list "Assert failed"))))))


;; assert function : fasle
;;
(define assert-false-lambda
  (CLambda (list 'check-false)
           (CIf (CId 'check-false)  (CError 'Exception (CStr (string->list "Assert failed"))) (CBool #t))))


;;  assert function :  equal
;;
(define assert-equal-lambda
  (CLambda (list 'left 'right)
           (CIf 
            (CCompare 'eq (CId 'left) (CId 'right))
            (CNone)
            (CRaise (CError 'Exception (CStr (string->list "Assertion Error")))))))



;;  assert function : is
;;
(define assert-is-lambda
  (CLambda (list 'left 'right)
           (CIf 
            ;(CCompare 'eq (CId 'left) (CId 'right))
            (CCompare 'is (CId 'left) (CId 'right))
            
            (CNone)
            (CRaise (CError 'Exception (CStr (string->list "Assertion Error")))))))



;;  assert function : in
;;
(define assert-in-lambda
  (CLambda (list 'left 'right)
           (CIf 
            
            (CCompare 'in (CId 'left) (CId 'right))
            
            (CNone)
            (CRaise (CError 'Exception (CStr (string->list "Assertion Error")))))))



;;  assert function : not in
;;
(define assert-notin-lambda
  (CLambda (list 'left 'right)
           (CIf 
            
            (CCompare 'notin (CId 'left) (CId 'right))
            
            (CNone)
            (CRaise (CError 'Exception (CStr (string->list "Assertion Error")))))))




;;  assert function : is not
;;
(define assert-isNot-lambda
  (CLambda (list 'left 'right)
           (CIf 
            ; (CCompare 'noteq (CId 'left) (CId 'right))
            (CCompare 'isnot (CId 'left) (CId 'right))
            
            (CNone)
            (CRaise (CError 'Exception (CStr (string->list "Assertion Error")))))))




;;  assert function : raise exception
;;
(define assert-raise-lambda
  (CLambda (list 'except 'expr  '--tuple-star-- '--tupled-value--)
           (CTryEx 
            (CApp (CId 'expr) (list (CId '--tupled-value--))) 
            (CIf 
             (CCompare 'eq
                       ; (CStr (symbol->string ....?????need suppoort arirty mistamtch in constructor
                       (CPrim1 'tagof  
                               (CApp (CId 'except) (list (CStr (string->list "Raise assertion"))))) 
                       (CPrim1 'tagof  (CId 'try-exception)))
             (CNone)
             ; (CApp (CId 'expr) (list (CId '--tupled-value--))) )
             (CRaise (CError 'Exception  (CPrim1 'tagof  (CId 'try-exception)))))
            (CError 'internal (CStr (string->list "really should not come here"))))))



;; predefined true
;;
(define true-val
  (CBool #t))


;; predefined  fasle
;;
(define false-val
  (CBool #f))


;; predefined  none
;;
(define none-val
  (CNone))


 

;; predefined exception : exception
;;
(define constructException
  (CLambda (list 'err-msg) 
           (CError 'Exception (CId 'err-msg))))


;; predefined exception :  ZeroDivisionError
;;
(define constructZeroDivisionError
  (CLambda (list 'err-msg) 
           (CError 'ZeroDivisionError (CId 'err-msg))))




;; predefined exception :  RuntimeError
;;
(define constructRuntimeError
  (CLambda (list 'err-msg) 
           (CError 'RuntimeError (CId 'err-msg))))





;; predefined exception : TypeError
;;
(define constructTypeError
  (CLambda (list 'err-msg) 
           (CError 'TypeError (CId 'err-msg))))


;; predefined exception : ValueError
;;
(define constructValueError
  (CLambda (list 'err-msg) 
           (CError 'ValueError (CId 'err-msg))))




;; predefined exception : AttributeError
;;
(define constructAttributeError
  (CLambda (list 'err-msg) 
           (CError 'AttributeError (CId 'err-msg))))



;; predefined exception : IndexError
;;
(define constructIndexError
  (CLambda (list ) 
           (CError 'IndexError (CStr (string->list "IndexError")))))



;; predefined exception : KeyError
;;
(define constructKeyError
  (CLambda (list 'err-msg) 
           (CError 'KeyError (CId 'err-msg))))



;; get length of object
;;
(define len-func
  (CLet 'dummy-func (CLambda (list) (CRaise (CError 'Exception (CStr (string->list "Shouldn't get here.")))))
        (CLambda (list 'lst)
                 (CIf (CEmpty? (CId 'lst))
                      (CNum 0)
                      (CBinOp 'add (CNum 1) (CApp (CId 'len) (list (CRest (CId 'lst)))))))))



;; get minimum object from list
;;
(define min-func
  (CLet 'dummy-func (CLambda (list) (CRaise (CError 'Exception (CStr (string->list "Shouldn't get here.")))))
        (CLambda (list 'lst)
                 
                 (CPrim1 'minimum (CId 'lst)))))



;; get max object from list
;;
(define max-func
  (CLet 'dummy-func (CLambda (list) (CRaise (CError 'Exception (CStr (string->list "Shouldn't get here.")))))
        (CLambda (list 'lst)
                 
                 (CPrim1 'maximum (CId 'lst)))))




;; get absolute value
;;
(define abs-func
  (CLet 'dummy-func (CLambda (list) (CRaise (CError 'Exception (CStr (string->list "Shouldn't get here.")))))
        (CLambda (list 'x)
                 (CIf (CCompare 'lt (CPrim1 'cast-int (CId 'x)) (CNum 0)) 
                      (CUnaryOp 'usub (CPrim1 'cast-int (CId 'x)))
                      (CPrim1 'cast-int (CId 'x))
                      ))))





;; detect if one is an instance of a class
;;
(define isinstance-func
  (CLambda (list 'i 'c)
           (CIsInstance (CId 'i) (CId 'c))))

#|

;;
;;
(define range-func
  (CLambda (list '--tuple-star-- 'x )
           (CRange (CId 'x))))
|#




;; predefined class : Int
;;
(define int_class
  (CClass 'int 
          (list 
           (CSet! (CId '__init__) (CLambda (list 'x)  (CPrim1 'cast-int (CId 'x))))
           (CSet! (CId '__pos__) (CLambda (list 'x)  (CPrim1 'cast-int (CId 'x))))
           (CSet! (CId '__neg__) (CLambda (list 'x)  (CUnaryOp 'usub (CPrim1 'cast-int (CId 'x)))))
           (CSet! (CId '__invert__) (CLambda (list 'x)  (CUnaryOp 'invert (CPrim1 'cast-int (CId 'x)))))
           (CSet! (CId '--not--)
                  (CLambda (list 'x)  
                           (CIf (CCompare 'eq (CPrim1 'cast-bool (CId 'x)) (CBool #t))  (CBool #t) (CBool #f)))))
          (list '__init__ '__pos__ '__neg__ '__invert__ '--not--)))



;; predefined class : bool
;;
(define bool_class
  (CClass 'bool 
          (list
           (CSet! (CId '__init__) (CLambda (list '--tuple-star-- 'x)  
                                           (CIf (CEmpty? (CId 'x) )  (CBool #f)   (CPrim1 'cast-bool (CFirst (CId 'x))))))
           (CSet! (CId '__pos__) (CLambda (list 'x)  (CPrim1 'cast-int (CId 'x))))
           (CSet! (CId '__neg__) (CLambda (list 'x)  (CUnaryOp 'usub (CPrim1 'cast-int (CId 'x)))))
           (CSet! (CId '__invert__) (CLambda (list 'x)  (CUnaryOp 'invert (CPrim1 'cast-int (CId 'x)))))
           (CSet! (CId '--not--) (CLambda (list 'x)  
                                          (CIf (CCompare 'eq (CId 'x) (CBool #t))  (CBool #f) (CBool #t)))))
          (list '__init__ '__pos__ '__neg__ '__invert__ '--not--)))



;; predefined class : string
;;
(define str_class
  (CClass 'str
          (list
           (CSet! (CId '__init__) (CLambda (list 'x )   (CPrim1 'cast-str (CId 'x)))))
          (list '__init__)))


;; predefined class : list
;;
(define list_class
  (CClass 'list
          (list
           (CSet! (CId '__init__) (CLambda (list  '--tuple-star-- 'x )   (CPrim1 'cast-list (CId 'x)))))
          (list '__init__)))


;; predefined class : tuple
;;
(define tuple_class
  (CClass 'tuple
          (list
           (CSet! (CId '__init__) (CLambda (list '--tuple-star-- 'x )   (CPrim1 'cast-tuple (CId 'x)))))
          (list '__init__)))


;; predefined class : set
;;
(define set_class
  (CClass 'set
          (list
           (CSet! (CId '__init__) (CLambda (list '--tuple-star-- 'x )   (CPrim1 'cast-set (CId 'x)))))
          (list '__init__)))


;; predefined class : range
;;
(define range_class
  (CClass 'range
          (list
           (CSet! (CId '__init__) (CLambda (list  '--tuple-star-- 'x )   
                                           
                                           ;   (CSeq
                                           ; (CPrim1 'print (CStr (string->list "adasdasda")))
                                           (CPrim1 'cast-range (CId 'x)))))
          (list '__init__)))



;; predefined class : bool dictionary
;;
(define dict_class
  (CClass 'dict
          (list
           (CSet! (CId '__init__) (CLambda (list 'x)   (CPass)))
           
           (CSet! (CId 'clear) (CLambda (list 'x ) 
                                        ; (CSeq 
                                        ;  (CPrim1 'print (CId 'x))
                                        (CCopy!   'x (CDict (list )   (list)))))
           
           (CSet! (CId 'keys) (CLambda (list 'x ) 
                                       (CDictKeys  (CId 'x))))
           
           (CSet! (CId 'items) (CLambda (list 'x ) 
                                        (CDictItems  (CId 'x))))
           
           (CSet! (CId 'values) (CLambda (list 'x ) 
                                         (CDictValues  (CId 'x))))
           
           (CSet! (CId 'get) (CLambda (list 's 'k '--default-none-- 'd ) 
                                      (CDictGet (CId 's) (CId 'k) (CId 'd))))
           
           (CSet! (CId 'update) (CLambda (list 'x  '--tuple-star-- 'd ) 
                                         (CDictUpdate (CId 'x) (CId 'd))))
           
           
           
           (CSet! (CId '__getitem__) (CLambda (list 's 'k '--default-none-- 'd ) 
                                              (CDictGet (CId 's) (CId 'k) (CId 'd))))
           
           
           )
          (list '__init__ 'clear 'get '__getitem__)))


; pair of symbol and CExp
;
(define-type LibBinding
  [bind (left : symbol) (right : CExp)])


;; the list of predefined things
;;
(define lib-functions
  (list (bind 'int int_class)
        (bind 'float int_class)
        (bind 'str str_class)
        (bind 'bool bool_class)
        (bind 'list list_class)
        (bind 'set set_class)
        (bind 'tuple tuple_class)
        (bind 'range range_class)
        (bind 'dict dict_class)
        (bind 'print print-lambda)
        (bind 'True true-val)
        (bind 'False false-val)
        (bind 'None none-val)
        (bind 'Exception constructException)
        (bind 'ZeroDivisionError constructZeroDivisionError)
        (bind 'IndexError constructIndexError)
        (bind 'TypeError constructTypeError)
        (bind 'AttributeError constructAttributeError)
        (bind 'KeyError constructKeyError)
        (bind 'ValueError constructValueError)
        (bind 'RuntimeError constructRuntimeError)
        (bind 'len len-func)
        (bind 'abs abs-func)
        (bind 'min min-func)
        (bind 'max max-func)
        (bind 'callable  callable-func)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-isNot-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-notin-lambda)
        (bind '___assertRaises  assert-raise-lambda)
        (bind 'isinstance  isinstance-func)
        ))


;; make recursive lib wrapping
;;
(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


