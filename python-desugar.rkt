#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

;; Hoists variables to the top of the desugared expression
;;
(define (desugar-wrap [expr : PyExpr]) : CExp
  (bind-CLets 
   (get-globals expr)
   (bind-CLets 
    (get-vars expr) 
    (desugar expr))))

;; Wraps an expression with CLets
;;
(define (bind-CLets [vars : (listof symbol)] [c : CExp]) : CExp
  (cond
    [(empty? vars)
     c]
    [else
     (CLet (first vars) (CUndefined) (bind-CLets (rest vars) c))]))

;; Create a sequence of CNonlocal from a list of symbols
;;
(define (make-nonlocals [vars : (listof symbol)]) : CExp
  (cond
    [(empty? vars) (CPass)]
    [(empty? (rest vars)) (CNonlocal (first vars))]
    [else (CSeq (CNonlocal (first vars)) (make-nonlocals (rest vars)))]))

;; Desugars the surface syntax into the core language
;;
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
                    (CLet 'temp-obj (desugar o)
                          (CApp (CGetField (CId 'temp-obj) field)  (cons (CId 'temp-obj) (map desugar args))))]
             [else
              (CApp (desugar f) (map desugar args))])]
    
    [PyId (x) (CId x)]
    
    [PyStr (s) (CStr (map first (map string->list s)))]
    
    [PyIf (t b e) 
          (CIf (desugar t) (desugar b)  (desugar e))]  
    
    [PyRaise (e)  (CRaise (desugar e))]
    
    [PyPass() (CPass)]
    
    [PyBoolOp (op args)(desugar-bool-op op args)]
    
    [PyBinOp (op l r) (CBinOp op (desugar l) (desugar r))]
    
    [PyDelete (p) (CDelete (desugar p))]
    
    [PyUnaryOp (op operand) 
               (cond 
                 [(symbol=? op 'not) (CUnaryOp op (desugar operand))]
                 [else (CLet 'temp-A (desugar operand)
                             (CApp 
                              (CGetField 
                               (CStrId (CPrim1 'tagof  (CId 'temp-A ))) op)
                              (list (CId 'temp-A))))])]
    
    
    [PyCompare (left ops comparators)
               (desugar-PyCompare left ops comparators)]
    
    [PyDict (keys values) (CDict (map desugar keys)  (map desugar values))]
    
    [PySet (values) (CSet  (map desugar values))]
    
    
    [PyLambda (ps b) (CLambda ps (desugar b))]
    
    [PySet! (lhs v) (desugar-PySet! lhs (desugar v))]
    
    [PyFunc (n ps es) (CSeq (CSet! (CId n) 
                                   (CUndefined))
                            (CSeq (CSet! (CId 'temp-func) 
                                         (CLambda ps 
                                                  (cond 
                                                    [(empty? es) (CPass)]
                                                    [else 
                                                     (local
                                                       ([define gbls (foldl append empty (map get-globals es))])
                                                       (bind-CLets 
                                                        (filter (lambda (x) (not (member x gbls))) 
                                                                (filter (lambda (x) (not (member x ps))) 
                                                                        (foldl append empty (map get-vars es))))
                                                        (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) 
                                                               (desugar (first es)) (rest es))))])))
                                  (CSet! (CId n) (CId 'temp-func))))]
    
    [PyClass (n b) 
             (local
               ([define body (map desugar b)]
                [define members (foldl append empty (map get-vars b))])
               (CSet! (CId n) (CClass n body members)))]

    [PyReturn (v) (CReturn (desugar v))]
    
    [PyDot (o f) (CGetField (desugar o) f)]
    
    [PyExpHandler (name  class_tag body)  (error 'desugar "syntax error (exh)") ]
    
    [PyTryExcept (try excepts orelse)
                 (CTryEx (desugar try)
                         (except-handler-build excepts) (desugar orelse))]
    
    [PyTryFinally (try final)
                  (CFinally (desugar try) (desugar final))]
    
    [PySlice (s  e step )
             (CSlice (desugar s) (desugar e) (desugar step)) 
             ]
    [PyList (l)
            (build-CList l #t)]
    
    [PyTuple (l)
             (build-CList l #f)]
    
    [PySubscript(o i)
                (CSubscript (desugar o) (desugar i))]
    
    [PyAugAssign(t op v)
                (CSet!
                 (desugar t)
                 
                 (CBinOp op  (desugar t)   (desugar v)))]
    
    [PyGlobal (xs) (CPass)]
    
    [PyNonlocal (xs) (make-nonlocals xs)]
    
    ;[else (error 'desugar   "dummy"  )]
    
    ))

;; Constructs a C-List from a list of expressions
;;
(define (build-CList [l : (listof PyExpr)] [m : boolean] ) : CExp
  (cond
    [(empty? l) (CList m)]
    [else 
     (CCons (desugar (first l)) (build-CList (rest l) m))])) 



(define (except-handler-build [ehs : (listof PyExpr)] ) : CExp
  (cond
    [(= 0 (length ehs)) (CRaise (CId 'try-exception))]
    [else
     (local
       ([define handler (first ehs)])
       (cond
         [(equal?  '--any-- (PyExpHandler-name handler))   
          (CLet (PyExpHandler-name handler) (CId 'try-exception) (desugar (PyExpHandler-body handler)))
          ]
         [else
          (CIf (CCompare 'eq (CPrim1 'tagof (CId 'try-exception)) (CStr (string->list(PyExpHandler-class_tag handler))))
               (CLet (PyExpHandler-name handler) (CId 'try-exception) (desugar (PyExpHandler-body handler)))
               (except-handler-build  (rest ehs)))]))]))



;; Returns a list of variables to hoist
;;
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
    [PySet! (lhs v) (map PyId-x (filter PyId? lhs)) ]
    ; [PyReturn  (v :  PyExpr)]
    [PyReturn (v) empty]
    ; [PyClass   (n : symbol) (b : (listof PyExpr))]))
    [PyClass (n b) empty]
    
    [PyDot (o f) empty]
    
    [PyTryExcept (t exhds e)
                 (append (foldl append (get-vars t) (map get-vars exhds)) (get-vars e))]
    
    [PyTryFinally (try final)
                  (append (get-vars try) (get-vars final))]
    
    [PyExpHandler (name class_tag  body)
                  (get-vars body)]
    
    [PyList(l) empty]
    
    [PyTuple(l) empty]
    
    [PySubscript(o i)
                empty]
    
    [PyDelete (d)
              empty]
    
    [PyAugAssign(t op v)
                (if (PyId? t)
                    (list (PyId-x t))
                    empty)]
    
    [PySlice (a b c)
             empty]
    
    [PySet(s)
          empty]
    
    [PyGlobal(x)
             empty]
    
    [PyNonlocal(x)
               empty]
    ;[else (error 'desugar   "need to hoist"  )]
    ))


;; Returns a list of globals to hoist
;; 
(define (get-globals [e : PyExpr]) : (listof symbol)
  (type-case PyExpr e
    ; [PySeq     (es : (listof PyExpr))]
    [PySeq (es)  (foldl append empty (map get-globals es))]
    ; [PyNum     (n : number)]
    [PyNum(n) empty]
    ; [PyId      (x : symbol)]
    [PyId(x) empty]
    ; [PyApp     (fun : PyExpr) (args : (listof PyExpr))]
    [PyApp(fun args) empty]
    ; [PyStr     (s : (listof string))]
    [PyStr(s) empty]
    ; [PyIf      (t : PyExpr) (b : PyExpr) (e : PyExpr)]
    [PyIf(t b e)  ( append (append (get-globals t) (get-globals b)) (get-globals e))]
    ; [PyRaise   (e : PyExpr)] 
    [PyRaise(e) empty]
    ; [PyPass]
    [PyPass() empty]
    ; [PyBoolOp  (s : symbol) (ls : (listof PyExpr))]
    [PyBoolOp (s ls) (foldl append empty (map get-globals ls))]   
    ; [PyUnaryOp (s : symbol) (operand : PyExpr)]
    [PyUnaryOp (s op) (get-globals op)]
    ; [PyBinOp   (s : symbol) (l : PyExpr) (r : PyExpr)]
    [PyBinOp (s l r) (append (get-globals l) (get-globals r))]
    ; [PyCompare (s : PyExpr) (ops : (listof symbol)) (comparators  : (listof PyExpr))]
    [PyCompare (s ops comparators) 
               (foldl append empty (map get-globals comparators))]
    ; [PyDict    (keys : (listof PyExpr)) (values : (listof PyExpr))]
    [PyDict (k v) empty]
    ; [PyLambda  (ps : (listof symbol)) (b : PyExpr)]
    [PyLambda (ps b) (get-globals b)]
    ; [PyFunc    (n : symbol) (ps : (listof symbol)) (b : (listof PyExpr))]
    [PyFunc(n ps b) (foldl append empty (map get-globals b))]
    ; [PySet!    (lhs : (listof symbol)) (v : PyExpr)]
    [PySet! (lhs v) empty ]
    ; [PyReturn  (v :  PyExpr)]
    [PyReturn (v) empty]
    ; [PyClass   (n : symbol) (b : (listof PyExpr))]))
    [PyClass (n b) (foldl append empty (map get-globals b))]
    
    [PyDot (o f) empty]
    
    [PyTryExcept (t exhds e)
                 (append (foldl append (get-globals t) (map get-globals exhds)) (get-globals e))]
    
    [PyTryFinally (try final)
                  (append (get-globals try) (get-globals final))]
    
    [PyExpHandler (name class_tag  body)
                  (get-globals body)]
    
    [PyList(l) empty]
    [PyTuple(l) empty]
    
    [PySubscript(o i)
                empty]
    
    [PyDelete (d)
              empty]
    
    [PyAugAssign(t op v)
                (get-globals t)
                ]
    [PySlice (a b c)
             empty]
    [PySet(s)
          empty]
    
    [PyGlobal(x)
             x]
    
    [PyNonlocal(x)
               empty]
    ;[else (error 'desugar   "need to hoist"  )]
    ))



;; Constructs a sequence of CSet!
;;
(define (desugar-PySet! [lhs : (listof PyExpr)] [e : CExp]) : CExp
  (cond 
    [(empty? lhs) (error 'desugar-PySet! "should not be here")]
    [(empty? (rest lhs)) (CSet! (desugar (first lhs)) e)]
    [else (CSeq (CSet! (desugar (first lhs)) e) (desugar-PySet!-aux (rest lhs)  (first lhs)))]))

;; Constructs a sequence of CSet!
;;
(define (desugar-PySet!-aux [lhs : (listof PyExpr)] [s : PyExpr]) : CExp
  (cond 
    [(empty? lhs) (error 'desugar-PySet! "should not be here")]
    [(empty? (rest lhs)) (CSet! (desugar (first lhs)) (desugar s))]
    [else (CSeq (CSet! (desugar (first lhs)) (desugar s)) (desugar-PySet!-aux (rest lhs) (first lhs)))]))

;; Constructs a sequence of CCompare
;;
(define (desugar-PyCompare [left : PyExpr] [ops : (listof symbol) ] [comparators : (listof  PyExpr)])  : CExp
  (cond 
    [(empty? ops) (error 'desugar-PyCompare "should not be here")]
    [(empty? (rest ops)) (CCompare (first ops) (desugar left) (desugar (first comparators)) )]
    [else
     (CBoolOp 'and (CCompare (first ops) (desugar left) (desugar (first comparators)) )
              (desugar-PyCompare (first comparators) (rest ops) (rest comparators)))]))

;; Constructs a sequence of CBoolOp
;;
(define (desugar-bool-op [op : symbol] [ls : (listof PyExpr)]) : CExp
  (cond
    [(empty? ls) (error 'desugar-bool-op "no operand")]
    [(empty? (rest ls)) (desugar (first ls))]
    [else (CBoolOp op (desugar (first ls) ) (desugar-bool-op op (rest ls)))]))