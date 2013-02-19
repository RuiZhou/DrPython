#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

(define (desugar [expr : PyExpr]) : CExp
  (type-case PyExpr expr
    [PySeq (es) 
           (cond 
             [(empty? es) (CPass)]
             [else (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))])]
    [PyNum (n) (CNum n)]
    [PyApp (f args)
           ;add self if the function is PyDot
           (type-case PyExpr f
             [PyDot (o field)
                    (CLet 'temp-obj (desugar f)
                          (CApp (CId 'temp-obj) (cons (desugar o) (map desugar args))))]
             [else
              (CApp (desugar f) (map desugar args))])]
    [PyId (x) (CId x)]
    [PyStr (s) (CStr s)]
    [PyIf (t b e) 
          (CIf (desugar t) (desugar b)  (desugar e))]  
    
    [PyRaise (e)  (CRaise (desugar e))]
    [PyPass() (CPass)]
    [PyBoolOp (op args)(desugar-bool-op op args)]
    [PyBinOp (op l r) (CBinOp op (desugar l) (desugar r))]
    [PyUnaryOp (op operand) (CUnaryOp op (desugar operand))] 
    [PyCompare (left ops comparators)
               (desugar-PyCompare left ops comparators)]
    [PyDict (keys values) (CDict (map desugar keys)  (map desugar values))]
    [PyLambda (ps b) (CLambda ps (desugar b))]
    [PySet! (lhs v) (desugar-PySet! lhs (desugar v))]
    [PyFunc (n ps es) (CSeq (CSet! n 
                                   (CLambda ps (CRaise (CError (CStr (list"D" "u" "m" "m" "y" " " "F" "u" "n" "c" "t" "i" "o" "n"))))))
                            (CSeq (CSet! 'temp-func (CLambda ps 
                                                             (cond 
                                                               [(empty? es) (CPass)]
                                                               [else (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))])))
                                  (CSet! n (CId 'temp-func))))]
    
    [PyClass (n b) (CClass n (desugar (PySeq b)) (foldl append empty (map get-vars b)))]
    
    
    [PyReturn (v) (CReturn (desugar v))]
    
    [PyDot (o f) (CGetField (desugar o) f)]
    ;[else (error 'desugar   "dummy"  )]
    
    ))

;;hoisting 
(define (get-vars [e : PyExpr]) : (listof symbol)
  (type-case PyExpr e
    ; [PySeq     (es : (listof PyExpr))]
    [PySeq (es)  (foldl append empty (map get-vars es))]
    ; [PyNum     (n : number)]
    [PyNum(n) empty]
    ; [PyId      (x : symbol)]
    [PyId(x) empty]
    ; [PyApp     (fun : PyExpr) (args : (listof PyExpr))]
    [PyApp(fun args) empty]
    ; [PyStr     (s : (listof string))]
    [PyStr(s) empty]
    ; [PyIf      (t : PyExpr) (b : PyExpr) (e : PyExpr)]
    [PyIf(t b e)  ( append (append (get-vars t) (get-vars b)) (get-vars e))]
    ; [PyRaise   (e : PyExpr)] 
    [PyRaise(e) empty]
    ; [PyPass]
    [PyPass() empty]
    ; [PyBoolOp  (s : symbol) (ls : (listof PyExpr))]
    [PyBoolOp (s ls) (foldl append empty (map get-vars ls))]   
    ; [PyUnaryOp (s : symbol) (operand : PyExpr)]
    [PyUnaryOp (s op) (get-vars op)]
    ; [PyBinOp   (s : symbol) (l : PyExpr) (r : PyExpr)]
    [PyBinOp (s l r) (append (get-vars l) (get-vars r))]
    ; [PyCompare (s : PyExpr) (ops : (listof symbol)) (comparators  : (listof PyExpr))]
    [PyCompare (s ops comparators) 
              (foldl append empty (map get-vars comparators))]
    ; [PyDict    (keys : (listof PyExpr)) (values : (listof PyExpr))]
    [PyDict (k v) empty]
    ; [PyLambda  (ps : (listof symbol)) (b : PyExpr)]
    [PyLambda (ps b) (get-vars b)]
    ; [PyFunc    (n : symbol) (ps : (listof symbol)) (b : (listof PyExpr))]
    [PyFunc(n ps b) empty]
    ; [PySet!    (lhs : (listof symbol)) (v : PyExpr)]
    [PySet! (lhs v) lhs]
    ; [PyReturn  (v :  PyExpr)]
    [PyReturn (v) empty]
    ; [PyClass   (n : symbol) (b : (listof PyExpr))]))
    [PyClass (n b) empty]
    
    [PyDot (o f) empty]
    ))
      
    
   
  


(define (desugar-PySet! [lhs : (listof symbol)] [e : CExp]) : CExp
  (cond 
    [(empty? lhs) (error 'desugar-PySet! "should not be here")]
    [(empty? (rest lhs)) (CSet! (first lhs) e)]
    [else (CSeq (CSet! (first lhs) e) (desugar-PySet!-aux (rest lhs) (first lhs)))]))


(define (desugar-PySet!-aux [lhs : (listof symbol)] [s : symbol]) : CExp
  (cond 
    [(empty? lhs) (error 'desugar-PySet! "should not be here")]
    [(empty? (rest lhs)) (CSet! (first lhs) (CId s))]
    [else (CSeq (CSet! (first lhs) (CId s)) (desugar-PySet!-aux (rest lhs) (first lhs)))]))


(define (desugar-PyCompare [left : PyExpr] [ops : (listof symbol) ] [comparators : (listof  PyExpr)])  : CExp
  (cond 
    [(empty? ops) (error 'desugar-PyCompare "should not be here")]
    
    [(empty? (rest ops)) (CCompare (first ops) (desugar left) (desugar (first comparators)) )]
    
    [else
     (CBoolOp 'and (CCompare (first ops) (desugar left) (desugar (first comparators)) )
              (desugar-PyCompare (first comparators) (rest ops) (rest comparators)))]))



(define (desugar-bool-op [op : symbol] [ls : (listof PyExpr)]) : CExp
  (cond
    [(empty? ls) (error 'desugar-bool-op "no operand")]
    [(empty? (rest ls)) (desugar (first ls))]
    [else (CBoolOp op (desugar (first ls) ) (desugar-bool-op op (rest ls)))]))