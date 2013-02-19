; primitives and print fucntions

#lang plai-typed

(require "python-core-syntax.rkt")
(require "python-helper.rkt")
(require (typed-in racket/base [display : (string -> void)]))
(require (typed-in racket (number->string : (number -> string))))

;pretty a CVAL
(define (pretty [arg : CVal]) : string
  (type-case CVal  arg
    [VNum (n) (to-string n)]
    [VStr (s) (list->string s)]
    [VClosure (env args body mems);
              ;(error 'prim "Can't print closures yet")]
              (constr (list "closure, args count:" (number->string (length args)) ))]
              
    [VBool (b) 
           (if b "true" "false")]
    [VNone() ""]
    [VDict(d) (constr (vdict-to-string d)) ]
    [VMembers(t m) "a object with members"]
    [VList(l m) (constr (list "[" (constr  (to-string-list l) )"]"))] 
    [VSet(s) (constr (vset-to-string s)) ]
    [VRange(a) (constr (list "Range object wrapping" (pretty a)) ) ]
    ))

;print a CVal, without location 
(define (pretty-no-loc [arg : CVal]) : string
  (type-case CVal  arg
    [VNum (n) (to-string n)]
    [VStr (s) (list->string s)]
    [VClosure (env args body mems);
              ;(error 'prim "Can't print closures yet")]
              (constr (list "closure, args count:" (number->string (length args)) ))]
              
    [VBool (b) 
           (if b "true" "false")]
    [VNone() ""]
    [VDict(d) (constr (vdict-to-string d)) ]
    [VMembers(t m) "a object with members"]
    [VList(l m) (constr (list "[" (constr  (to-string-list-no-loc l) )"]"))]
    [VSet(s) (constr (vset-to-string-no-loc s)) ]
    [VRange(a) (constr (list "Range object wrapping" (pretty-no-loc a)) )]
    ))

; expand recursive list to flat string list
(define (to-string-list [l : ListVal]) : (listof string)
  (cond
    [(mt? l) (list)]
    [(mt? (node-next l)) (list (pretty (VObj-value (node-value l))))]
    [else
     (cons  (constr (list (pretty (VObj-value (node-value l)))  ", " )) (to-string-list (node-next l)))]))

; expand recursive list to flat string list, without location
(define (to-string-list-no-loc [l : ListVal]) : (listof string)
  (cond
    [(mt? l) (list)]
    [(mt? (node-next l)) (list (pretty-no-loc (VObj-value (node-value l))))]
    [else
     (cons  (constr (list (pretty (VObj-value (node-value l)))  ", " )) (to-string-list-no-loc  (node-next l)))]))


; expand  VDict to flat string list
(define (vdict-to-string [d : (hashof VObjType VObjType) ]) : (listof string)
  (local ([define keys      (hash-keys d)])
    (cond
      [(empty? keys) (list "")]
      [else
       (cons (constr
              (list
             ;  "items count" 
              ; (number->string (length keys))
               "\n {"
               (pretty-obj   (first keys))
               " : "
               (pretty-obj (some-v (hash-ref d (first keys)))) 
               " } "
               ))
             (vdict-to-string (hash-remove d (first keys))))])))

; expand  VDict to flat string list,   without location
(define (vdict-to-string-no-loc [d : (hashof VObjType VObjType) ]) : (listof string)
  (local ([define keys      (hash-keys d)])
    (cond
      [(empty? keys) (list "")]
      [else
       (cons (constr
              (list
             ;  "items count" 
              ; (number->string (length keys))
               "\n {"
               (pretty-obj-no-loc   (first keys))
               " : "
               (pretty-obj-no-loc  (some-v (hash-ref d (first keys)))) 
               " } "
               ))
             (vdict-to-string-no-loc (hash-remove d (first keys))))])))


; expand  Vset to flat string list
(define (vset-to-string [d : (hashof CVal VObjType) ]) : (listof string)
  (local ([define keys      (hash-keys d)])
    (cond
      [(empty? keys) (list "")]
      [else
       (cons (constr
              (list
             ;  "items count" 
              ; (number->string (length keys))
               "\n {"
               (pretty   (first keys))
               " : "
               (pretty-obj (some-v (hash-ref d (first keys)))) 
               " } "
               ))
             (vset-to-string (hash-remove d (first keys))))])))

; expand  VSet to flat string list,   without location
(define (vset-to-string-no-loc [d : (hashof CVal VObjType) ]) : (listof string)
  (local ([define keys      (hash-keys d)])
    (cond
      [(empty? keys) (list "")]
      [else
       (cons (constr
              (list
             ;  "items count" 
              ; (number->string (length keys))
               "\n {"
               (pretty-no-loc   (first keys))
               " : "
               (pretty-obj-no-loc (some-v (hash-ref d (first keys)))) 
               " } "
               ))
             (vset-to-string-no-loc (hash-remove d (first keys))))])))



;print an VObjType nicely         
(define (pretty-obj [arg : VObjType]) : string
  (cond 
    [(symbol=? (VObj-type arg) 'NoneType) 
     "None\n"]
    [else 
     (constr ( list  "<" 
                     (symbol->string (VObj-type arg))
                     "("
                     (number->string (VObj-class_loc arg))
                     ")"
                     " object at "
                     (number->string (VObj-id arg))
                     " with value ("
                     (pretty (VObj-value arg))
                     ")>"))]))

;print an VObjType nicely, without location info
(define (pretty-obj-no-loc [arg : VObjType]) : string
  (cond 
    [(symbol=? (VObj-type arg) 'NoneType) 
     "None\n"]
    [else 
     (constr ( list  "<" 
                     (symbol->string (VObj-type arg))
                     "("
                     (number->string (VObj-class_loc arg))
                     ")"
                     " object at x"
                      
                     " with value ("
                     (pretty-no-loc (VObj-value arg))
                     ")>") )]))


;print an VObjType 
(define (print [arg : VObjType]) : void
  (display (pretty-obj arg)))

;number to string
(define (n->s [num : number]) : string
  (local ([define  lsd  (modulo num 10)])
    (local ([define lsdchar 
              (cond 
                [(= lsd 0) "0"]
                [(= lsd 1) "1"]
                [(= lsd 2) "2"]
                [(= lsd 3) "3"]
                [(= lsd 4) "4"]
                [(= lsd 5) "5"]
                [(= lsd 6) "6"]
                [(= lsd 7) "7"]
                [(= lsd 8) "8"]
                [(= lsd 9) "9"])])
      (cond 
        [(= 0 (floor (/ num 10)))
         lsdchar]
        [else
         (string-append (n->s (floor (/ num 10))) lsdchar)]))))