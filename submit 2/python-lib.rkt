#lang plai-typed

(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))

(define print-lambda
  (CLambda (list 'to-print)
           (CPrim1 'print (CId 'to-print))))
#|
(define assert-true-lambda
  (CFunc (list 'check-true)
    (CIf (CId 'check-true) (CTrue) (CError (CStr "Assert failed")))))
|#

(define assert-equal-lambda
  (CLambda (list 'left 'right)
           (CIf 
            (CCompare 'eq (CId 'left) (CId 'right))
            (CNone)
            (CRaise (CError (CStr (list"A" "s" "s" "e" "r" "t" "i" "o" "n" " " "E" "r" "r" "o" "r")))))))

(define true-val
  (CBool #t))

(define false-val
  (CBool #f))

(define none-val
  (CNone))


(define constructException
  (CLambda (list 'err-msg) 
           (CError (CId 'err-msg))))

(define len-func
  (CLet 'dummy-func (CLambda (list) (CRaise (CError (CStr (list "S" "houldn't get here.")))))
        (CLambda (list 'str)
                 (CIf (CEmpty? (CId 'str))
                      (CNum 0)
                      (CBinOp 'add (CNum 1) (CApp (CId 'len) (list (CRest (CId 'str)))))))))
  
  
(define int_class
  (CClass 'int (CNone) (list)))


  (define-type LibBinding
    [bind (left : symbol) (right : CExp)])
  
  (define lib-functions
    (list (bind 'int int_class)
          (bind 'print print-lambda)
          (bind 'True true-val)
          (bind 'False false-val)
          (bind 'None none-val)
          (bind 'Exception constructException)
          (bind 'len len-func)
          ; (bind '___assertTrue assert-true-lambda)
          (bind '___assertEqual assert-equal-lambda)
          (bind '___assertIs assert-equal-lambda)
          
          ))
  
  (define (python-lib expr)
    (local [(define (python-lib/recur libs)
              (cond [(empty? libs) expr]
                    [(cons? libs)
                     (type-case LibBinding (first libs)
                       (bind (name value)
                             (CLet name value
                                   (python-lib/recur (rest libs)))))]))]
      (python-lib/recur lib-functions)))
  
  
  