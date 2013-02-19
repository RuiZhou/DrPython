;; This is the interpreter &  associated functions
#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-helper.rkt"
         "python-lib.rkt")

(require (typed-in racket (string<? : (string string -> boolean))))
(require (typed-in racket (string>? : (string string -> boolean))))
(require (typed-in racket (string<=? : (string string -> boolean))))
(require (typed-in racket (string>=? : (string string -> boolean))))
(require (typed-in racket (number->string : (number -> string))))

; debug system

;; The CExp last interped
;;
(define last-interped (CNone))


;; verify the store, make sure id and loc match
;;
(define (verify-store [st : (hashof Location VObjType)] [s : string]) : string
  (verify-store-aux (hash-keys st) st s))


;; verify store helper
;;
(define (verify-store-aux [keys : (listof Location)] [st : (hashof Location VObjType)] [s : string]) : string
  (cond
    [(empty? keys) s]
    [(and
      ;ignore
      (not (equal? (first keys) (VObj-id (some-v (hash-ref store (first keys))))))
      (not (equal? 'undefined (VObj-type (some-v (hash-ref store (first keys))))))
      (not (isSimpleType (VObj-type (some-v (hash-ref store (first keys))))))
      ;select
      (equal? 'dict (VObj-type (some-v (hash-ref store (first keys))))))
     (verify-store-aux (rest keys) st  
                       (constr (list s "loc-err :"
                                     (number->string (first keys))
                                     "<>"
                                     (pretty-obj (some-v (hash-ref st (first keys))))
                                     "\n")))]
    [else 
     (verify-store-aux (rest keys) st s)]))



;; wrapper for interp , to get fall through exceptions
;;
(define (wrap-interp [v-answer : VAnswer]) : void
  (type-case VAnswer v-answer
    [VResult (v) (void)]
    [VReturn (v) (error 'interp "Syntax Error1")]
    [VError (v) 
            (error 'interp 
                   (constr (list "\nException Fall through:\n" 
                                 "Ex Form: " (symbol->string (VObj-type v)) 
                                 "\nEx Msg: " 
                                 (list->string (VStr-s (VObj-value v))) "\nStack:")))]))



;(;define (hash-set-update-obj!  [s : (hashof Location VObjType)] [l : Location] [v : VObjType]) : void
;(begin
;(set! v (VObj 
;l
;(VObj-type v)
;(VObj-class_loc v)
;(VObj-value v)))
;(hash-set! s l v)))

;; is this type predefined?
;;
(define (isPredefined [x : symbol]) : boolean
  (or
   (equal? x 'str)
   (equal? x 'int)
   (equal? x 'float)
   (equal? x 'bool)
   (equal? x 'list)
   (equal? x 'tuple)
   (equal? x 'set)
   (equal? x 'dict)
   (equal? x 'range)))


;; Is this type simple?
;;
(define (isSimpleType [x : symbol]) : boolean
  (or
   (equal? x 'str)
   (equal? x 'int)
   (equal? x 'bool)
   (equal? x 'float)))



;; expand a ListVal from recursive to flat
;;
(define (ListVal->list [tp : ListVal]) : (listof VAnswer) 
  (cond
    [(mt? tp) (list)]
    [else
     (cons (VResult (node-value tp )) (ListVal->list (node-next tp)))]))



;; get the arguements values (potentially in a recursive tuple) to flat lists
;;
(define (get-arg-values [x : (listof symbol)] [args : (listof CExp)]) : (listof VAnswer)
  (cond
    ;[(empty? x) (list)]
    [(empty? args) (list)]
    [(CId? (first args)) 
     (if
      (equal? (CId-x (first args)) '--tupled-value--)
      (begin
        ;(display x)
        ;(display args)
        ;(display "\n")
        (ListVal->list (VList-l (VObj-value (VResult-v (interp (first args))))))) 
      (begin
        ;(display "haha")
        (map interp args)))]
    [else
     (begin
       ;(display "haha")
       (map interp args))]))



;; Interp CExp to VAnswer
;;
(define (interp [expr : CExp]) : VAnswer
  (begin
    #|(local ([define vr (verify-store store "")]) 
 (if
  (not (equal? "" vr))
  (begin 
   (display "\n\n!!!!!CExp\n")
   (display  last-interped)
   (display "\n messed store :\n")
   (display " before execute :\n")
   (display expr)
   (display " messed value :\n")
   (display vr)
   (error '- ""))
   (set! last-interped expr)))|#
    (local 
      ([define env (first (unbox menv))])
      (type-case CExp expr
        [CNum (n) (VResult (make-bind-VObj 'int (VObj-id (VResult-v (lookup 'int (unbox menv)))) (VNum n)))]
        [CStr (s) 
              ;(begin (print-envs (unbox menv))
              (VResult (make-bind-VObj 'str (VObj-id (VResult-v (lookup 'str (unbox menv)))) (VStr s)))]  
        [CError (tag e) 
                (local 
                  ([define exp (interp e)])
                  (type-case VAnswer exp
                    [VResult (v) (VResult (make-bind-VObj tag not_in_store (VObj-value (VResult-v exp))))]
                    [VReturn (v) (error 'interp "Syntax Error2")]
                    [VError (v) exp]))] ;TODO
        [CIf (t b e) 
             (local ([define test-answer (interp t)])
               (type-case VAnswer test-answer
                 [VResult (v) (cond
                                [(isTrue (VResult-v test-answer)) (interp b)]
                                [else (interp e)])]
                 [VReturn (v) (error 'interp "Syntax Error3")]
                 [VError (v) test-answer]))]
        [CId (x)
             (begin
               ;(display "LOOK FOR ") 
               ;(display x) 
               ;(display "\n") 
               ;(display "envs:\n") 
               ;(print-envs (unbox menv))
               ;(display "store:\n")
               ;(print-store)
               ;(display "\n")
               (lookup x (unbox menv)))]
        #|
   [CId (x)
     (type-case VAnswer (lookup x (unbox menv))
       [VResult (v) 
        (VObj-]
       [VReturn (v) (error 'interp "Syntax Error3")]
       [VError (v) test-answer]))]
   |#
        [CLet (x bind body)
              (local
                ([define exp (interp bind)])
                (type-case VAnswer exp
                  [VReturn (v) (error 'interp "Syntax Error4")]
                  [VError (v) exp]
                  [VResult (v) 
                           (if
                            (isSimpleType (VObj-type v))
                            (local
                              ([define loc (next-loc)]
                               [define new-env (hash-set env x loc)])
                              (begin  
                                (push-frame new-env)
                                (hash-set! store loc v)
                                (local
                                  ([define value (interp body)])
                                  (begin
                                    (pop-frame)
                                    value))))
                            (begin
                              (push-frame (hash-set env x (VObj-id v)))
                              (local
                                ([define value (interp body)])
                                (begin
                                  (pop-frame)
                                  value))))]))]
        [CSeq (e1 e2)
              (local ([define e1-value (interp e1)])
                (type-case VAnswer e1-value
                  [VReturn (v) (VResult v)]
                  [VError (v) e1-value]
                  [VResult (v) (interp e2)]))]
        
        [CApp (fun arges)
              (begin
                ;(display  "\ncalling : ")
                ;(display fun)
                ;(display", with : " )
                ;(display arges)
                ;(display "\n")
                ;(display " envs: \n")
                ;(print-envs (unbox menv))
                ;(display " store: \n")
                ;(print-store)
                (local
                  ([define funAnswer (interp fun)])
                  (type-case VAnswer funAnswer
                    [VReturn (v) (error 'interp "Syntax Error5")]
                    [VError (v) funAnswer]
                    [VResult (funObj) 
                             (cond 
                               [(and (CId? fun)
                                     (isPredefined (CId-x fun)))
                                (begin 
                                  ;(display (constr (list "constructing : " 
                                  ;(symbol->string (CId-x fun)) ", with : " )))
                                  ;(display arges)
                                  ;(display "\n")
                                  ;(display
                                  (interp  
                                   (CApp 
                                    (CGetField fun '__init__)
                                    arges))
                                  ;))
                                  ;(display "\n")
                                  ;(error 'interp "??????????")
                                  )]
                               
                               [;(symbol=? (VObj-type funObj) 'func);a lamba function
                                else
                                (begin 
                                  ;(display  "calling : ")
                                  ;(display fun)
                                  ;(display", with : " )
                                  ;(display arges)
                                  ;(display "\n")
                                  (type-case CVal (VObj-value funObj)
                                    [VClosure (e a b m)
                                              (local 
                                                [(define arg-answers 
                                                   (get-arg-values 
                                                    (VClosure-args (VObj-value (VResult-v funAnswer))) arges))
                                                 ;(map interp arges))
                                                 (define bad-syntax (filter VReturn? arg-answers))
                                                (define errors (filter VError? arg-answers))]
                                                (cond
                                                  [(not (empty? bad-syntax )) (error 'interp "Syntax Error6")]
                                                  [(not (empty? errors )) (first errors)]
                                                  [else (local 
                                                          [(define argvs (map VResult-v arg-answers))]
                                                          #|
                          (begin
                           (push-frame 
                            (bind-args a argvs e))
                           (local
                            ([define value (interp b)])
                            (begin 
                             (pop-frame)
                             (type-case VAnswer value
                               [VReturn (v) (VResult v)]
                               [VError (v) value]
                               [VResult (v) value]))))
                           
                           |#
                                                          (type-case EnvOrError (bind-args a argvs e)
                                                            [isEnv(ise)
                                                                  (begin
                                                                    (push-frame   ise)
                                                                    (local
                                                                      ([define value (interp b)])
                                                                      (begin 
                                                                        (pop-frame)
                                                                        (type-case VAnswer value
                                                                          [VReturn (v) (VResult v)]
                                                                          [VError (v) value]
                                                                          [VResult (v) value]))))]
                                                            [isErr(ise) ise]))]))]
                                    [else (error 'interp "Not a closure")]))])])))]
        
        [CLambda (args body) (VResult (make-bind-VObj 'func not_in_store (VClosure env args body (hash (list)))))] 
        
        [CPrim1 (prim arg)
                (local 
                  ([define arg-answer (interp arg)]) 
                  (type-case VAnswer arg-answer
                    [VReturn (v) (error 'interp-cprim1 "Syntax Error7")]
                    [VError (v) arg-answer]
                    [VResult (v)
                             (begin
                               ;(display "hello") 
                               ;(display prim)
                               ;(print v)
                               (interp-prim1-aux prim (VResult-v arg-answer)))]))]
        
        [CBool (b) (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool b)))]
        
        [CRaise (e) 
                (local 
                  ([define e-answer (interp e)])
                  (type-case VAnswer e-answer
                    [VReturn (v) (error 'interp "Syntax Error8")]
                    [VError (v) e-answer]
                    [VResult (v) (VError (VResult-v e-answer))]))]
        
        [CPass() (VResult (make-bind-VObj 'NoneType not_in_store (VNone)))] 
        
        [CBoolOp (op l r) (interp-CBoolOp op l r)] 
        
        [CUnaryOp (op operand) (interp-CUnaryOp op operand)]
        
        [CBinOp (op l r) (interp-CBinOp op l r)]
        
        [CEmpty? (l)
                 (local 
                   ([define l-answer (interp l)])
                   (type-case VAnswer l-answer
                     [VReturn (v) (error 'interp "Syntax Error9")]
                     [VError (v) l-answer]
                     [VResult (v) 
                              (VResult 
                               (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                               (VBool 
                                                (cond 
                                                  [(VStr? (VObj-value v))
                                                   (empty? (VStr-s (VObj-value v)))]
                                                  [(VList? (VObj-value v))
                                                   (mt? (VList-l (VObj-value v)))]
                                                  [(VDict? (VObj-value v))
                                                   (empty? (hash-keys (VDict-d (VObj-value v))))]))))]))]
        
        [CFirst (l)
                (local 
                  ([define l-answer (interp l)])
                  (type-case VAnswer l-answer
                    [VReturn (v) (error 'interp "Syntax Error10")]
                    [VError (v) l-answer]
                    [VResult (v) (VResult
                                  (if 
                                   (VStr? (VObj-value v))
                                   (make-bind-VObj 'str (VObj-id (VResult-v (lookup 'str (unbox menv)))) 
                                                   (VStr (list (first (VStr-s (VObj-value v))))))
                                   (node-value (VList-l (VObj-value v)))))]))]
        [CRest (l)
               (local 
                 ([define l-answer (interp l)])
                 (type-case VAnswer l-answer
                   [VReturn (v) (error 'interp "Syntax Error11")]
                   [VError (v) l-answer]
                   [VResult (v) (VResult 
                                 (cond 
                                   [(VStr? (VObj-value v))
                                    (make-bind-VObj 'str 
                                                    (VObj-id (VResult-v (lookup 'str (unbox menv)))) 
                                                    (VStr (rest (VStr-s (VObj-value v)))))]
                                   [(VList? (VObj-value v))
                                    (make-bind-VObj 'list (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                                    (VList (node-next (VList-l (VObj-value v))) 
                                                           (VList-m (VObj-value v))))]
                                   [(VDict? (VObj-value v))
                                    (make-bind-VObj 'dict not_in_store 
                                                    (VDict
                                                     (hash-remove (VDict-d (VObj-value v)) 
                                                                  (first (hash-keys (VDict-d (VObj-value v)))))))]
                                   [else
                                    (error 'crest "check rest")]))]))]
        [CCompare (op left right)
                  (interp-CCompare op left right)]
        
        [CNone() (VResult (make-bind-VObj 'NoneType not_in_store (VNone)))]
        
        [CDict (keys values)
               (interp-CDict keys values (hash empty))]
        
        [CSet! (lhs-e v)
               (local ([define v-answer (interp v)])
                 (cond
                   [(CId? lhs-e)
                    (local ([define lhs (CId-x lhs-e)])
                      (type-case VAnswer v-answer 
                        [VReturn (v) (error 'interp "Syntax Error12")]
                        [VError (v) v-answer]
                        [VResult (value)
                                 (if (isSimpleType (VObj-type value))
                                     (type-case (optionof Location) (hash-ref env lhs)
                                       [some (loc) 
                                             (begin
                                               (hash-set! store loc value)
                                               v-answer)]
                                       [none () (local
                                                  ([define newloc (next-loc)]
                                                   [define new-env (hash-set env lhs newloc)])
                                                  (begin
                                                    (update-frame new-env )
                                                    (hash-set! store newloc value)
                                                    v-answer))])
                                     (type-case (optionof Location) (hash-ref env lhs)
                                       [some (loc) 
                                             (type-case (optionof VObjType) (hash-ref store loc)
                                               [some (obj) 
                                                     (if (equal? (VObj-type obj) 'undefined)
                                                         (begin
                                                           ;(display "heres?")
                                                           ;(display lhs)
                                                           ;(display "\n")
                                                           (hash-set! store loc 
                                                                      (VObj loc
                                                                            (VObj-type value)
                                                                            (VObj-class_loc value)
                                                                            (VObj-value value)
                                                                            )) v-answer)
                                                         (local
                                                           ([define new-env (hash-set env lhs (VObj-id value))])
                                                           (begin
                                                             ;(display "here?")
                                                             (update-frame new-env )
                                                             v-answer)))]
                                               [none () 
                                                     (local
                                                       ([define new-env (hash-set env lhs (VObj-id value))])
                                                       (begin
                                                         ;(display "herea?")
                                                         (update-frame new-env )
                                                         v-answer))])]
                                       [none () 
                                             (local
                                               ([define new-env (hash-set env lhs (VObj-id value))])
                                               (begin
                                                 (update-frame new-env )
                                                 v-answer))]))]))]
                   [(CSubscript? lhs-e)
                    (begin
                      ;(display "\ntrying to set ")
                      ;(display (CSubscript-object lhs-e))
                      ;(display "\ntrying to set ")
                      ;(display (interp (CSubscript-object lhs-e)))
                      ;(display "\nindex ")
                      ;(display (CSubscript-index lhs-e))
                      ;(display "\nvalue ")
                      ;(display v-answer)
                      ;set
                      (local ([define object (interp (CSubscript-object lhs-e))]
                              [define index (interp (CSubscript-index lhs-e))]
                             (define value v-answer))
                        (type-case VAnswer object
                          [VError(err) object]
                          [VReturn(ret) (mkerr "11")]
                          [VResult(oo)
                                  (type-case VAnswer index
                                    [VError(err) index]
                                    [VReturn(ret) (mkerr "22")]
                                    [VResult(ii)
                                            (type-case VAnswer v-answer
                                              [VError(err) v-answer]
                                              [VReturn(ret) (mkerr "22")]
                                              [VResult(vv) 
                                                      (cond
                                                        [(VDict? (VObj-value oo))
                                                         (begin
                                                           ;(display "\nenvz;\n")
                                                           ;(print-envs (unbox menv))
                                                           ;(display "\nenv...;\n")
                                                           ;(display "\n store\n")
                                                           ;(print-store)
                                                           ;(display "\n store...\n")
                                                           ;(display oo)
                                                           ;(display "\n")
                                                           ;(display ii)
                                                           ;(display vv)
                                                           ;(display "\nhere\n")
                                                           (hash-set!
                                                            store
                                                            (VObj-id oo)
                                                            (VObj 
                                                             (VObj-id oo)
                                                             (VObj-type oo)
                                                             (VObj-class_loc oo)
                                                             (VDict (hash-set (VDict-d (VObj-value oo)) ii vv))))
                                                           (interp (CSubscript-object lhs-e)))]
                                                        [else (error '?? "adadasdasda")])])])])))]
                   
                   [else
                    (error 'set "immediate value")]))]
        
        [CDelete (target )
                 (cond
                   [(CSubscript? target)
                    (begin
                      (local ([define object (interp (CSubscript-object target))]
                              [define index (interp (CSubscript-index target))])
                        (type-case VAnswer object
                          [VError(err) object]
                          [VReturn(ret) (mkerr "11")]
                          [VResult(oo)
                                  (type-case VAnswer index
                                    [VError(err) index]
                                    [VReturn(ret) (mkerr "22")]
                                    [VResult(ii)
                                            (cond
                                              [(VDict? (VObj-value oo))
                                               (begin
                                                 ;(display "\nenvz;\n")
                                                 ;(print-envs (unbox menv))
                                                 ;(display "\nenv...;\n")
                                                 ;(display "\n store\n")
                                                 ;(print-store)
                                                 ;(display "\n store...\n")
                                                 ;(display oo)
                                                 ;(display "\n")
                                                 ;(display ii)
                                                 ;(display vv)
                                                 ;(display "\nhere\n")
                                                 (hash-set!
                                                  store
                                                  (VObj-id oo)
                                                  (VObj 
                                                   (VObj-id oo)
                                                   (VObj-type oo)
                                                   (VObj-class_loc oo)
                                                   (VDict (hash-remove (VDict-d (VObj-value oo)) ii ))))
                                                 (interp (CSubscript-object target)))]
                                              [else
                                               (error '?? "adadasdasda")])])])))]
                   [else
                    (error '?? "adadasdasda")])]
        [CCopy! (lhs v)
                (local ([define v-answer (interp v)])
                  (type-case VAnswer v-answer 
                    [VReturn (v) (error 'interp "Syntax Error12")]
                    [VError (v) v-answer]
                    [VResult (value)
                             (type-case (optionof Location) (hash-ref env lhs)
                               [some (loc) 
                                     (begin
                                       (hash-set! store loc 
                                                  (VObj loc 
                                                        (VObj-type value)
                                                        (VObj-class_loc value)
                                                        (VObj-value value)))
                                       v-answer)]
                               [none () (local
                                          ([define newloc (next-loc)]
                                           [define new-env (hash-set env lhs newloc)])
                                          (begin
                                            (update-frame new-env )
                                            (hash-set! store newloc (VObj newloc 
                                                                          (VObj-type value)
                                                                          (VObj-class_loc value)
                                                                          (VObj-value value)))
                                            v-answer))])]))]
        
        [CReturn (v)
                 (local ([define v-answer (interp v)])
                   (type-case VAnswer v-answer
                     [VResult (v) (VReturn v)]
                     [VReturn (v) (error 'interp "Syntax Error13")]
                     [VError (v) v-answer]))]
        [CStrId(p) 
               (local ([define p-answer (interp p)])
                 (type-case VAnswer p-answer
                   [VResult (v) (lookup 
                                 (string->symbol (list->string (VStr-s (VObj-value v))))
                                 (unbox menv))]
                   [VReturn (v) (error 'interp "Syntax Error13_2")]
                   [VError (v) p-answer]))]
        
        [CGetField (o s)
                   (local ([define v-answer (interp o)])
                     (type-case VAnswer v-answer
                       [VResult (v) 
                                (type-case CVal (VObj-value v)
                                  [VClosure (env args body members)
                                            (lookup s (list members))]
                                  [VMembers (ct m);user defined
                                            (local 
                                              ([define vr (some-v (hash-ref store (VObj-class_loc v)))]) ;TODO!
                                              ;(lookup (VObj-type v) (unbox menv))])
                                              ;(type-case VAnswer vclass
                                              ;[VError (er) vclass] 
                                              ;[VReturn (er) (error 'interp "not found ")]
                                              ;[VResult(vr)
                                              (lookup s (list m (VClosure-members (VObj-value vr)))))]
                                  [else 
                                   (begin
                                     ;(display "looking for :")
                                     ;(display (VObj-class_loc v))
                                     ;(print-store)
                                     (local 
                                       ([define vclass ;(hash-ref store (VObj-class_loc v))]) TODO!
                                          (lookup (VObj-type v) (unbox menv))])
                                       (type-case VAnswer vclass
                                         [VError (er) vclass] 
                                         [VReturn (er) (error 'interp "not found ")]
                                         [VResult(vr)
                                                 (lookup s (list (VClosure-members (VObj-value vr))))])))])]
                       [VReturn (v) (error 'interp "Syntax Error15")]
                       [VError (v) v-answer]))]
        
        ;Interp [CClass (n : symbol) (b : CExp) (vs : (listof symbol))]    
        ;to 
        ;[VClosure (env : Env) (args : (listof symbol)) (body : CExp) (members : Env)]
        [CClass (n b vs)
                (begin
                  ;(print-envs (unbox menv))
                  ;(display "env:\n")
                  ;(print-env env)
                  ;(display "\n")
                  ;(print-store)
                  (local 
                    ;class tag ?? just use the symbol?
                    ;([define class_tag (string->symbol (string-append "class: " (symbol->string n)))])
                    ([define class_loc (next-loc)]
                     [define class_tag n])
                    (VResult
                     (local ([define obj 
                               (VObj class_loc 'type;class ... 
                                     not_in_store 
                                     (local
                                       ;the actual colosure which includes the constructor function and the 
                                       ;static memebers
                                       ([define constructor
                                          (VClosure 
                                           env ;make a copy of current VAnswerenvironment
                                           empty;empty means take no parameter in constructor
                                           (CInstance class_tag class_loc) ;body to make a new object;TODO need fix
                                           (begin ;the static members
                                             ;now add all the hosted variable as undefined
                                             ;(begin (add-undefined-to-store)
                                             ;(pln "Class" n)
                                             (local ([define v-answer (interp-body b env (hash (list)))])
                                               (type-case EnvOrError v-answer
                                                 [isEnv (v) (begin 
                                                              ;(display "\n") 
                                                              ;(display v) 
                                                              ;(display "\n") 
                                                              v )]
                                                 [isErr (v) 
                                                        (begin (display v)
                                                               (error 'interp "Syntax Error162"))]))))])
                                       (begin constructor)))])
                       (begin
                         (hash-set! store class_loc obj)
                         ;(display "\n") 
                         ;(display obj) 
                         ;(display "\n")
                         obj)))))]
        #|
   ;Interp [CClass (n : symbol) (b : CExp) (vs : (listof symbol))]    
   ;to 
   ;[VClosure (env : Env) (args : (listof symbol)) (body : CExp) (members : Env)]
   [CClass (n b vs)
      (local 
        ;class tag ?? just use the symbol?
        ;([define class_tag (string->symbol (string-append "class: " (symbol->string n)))])
       ([define class_tag n])
       (local ([define loc (next-loc)])
       (VResult
        (make-bind-VObj 'type;class ... 
            not_in_store 
           (local
             ;the actual colosure which includes the constructor function and the 
             ;static memebers
            ([define constructor
             (VClosure 
               env ;make a copy of current VAnswerenvironment
               empty;empty means take no parameter in constructor
              (CInstance class_tag loc) ;body to make a new object
              (begin ;the static members
                ;first we push a empty env
               (push-frame (hash empty))
                ;now add all the hosted variable as undefined
                ;(begin (add-undefined-to-store)
                  (begin (bind-hoisted-variables vs)
                      (local ([define v-answer (interp b)])
                       (type-case VAnswer v-answer
                         [VResult (v) (first (unbox menv))]
                         [VReturn (v) (error 'interp "Syntax Errorc1")]
                         [VError (v) 
                        (begin (display b)
                        (error 'interp "Syntax Error162"))])))))])
            (begin (pop-frame) ;pop the old frame
                ;(local ([define loc (next-loc)])
                (begin;bind the classinto env 
                 (update-frame 
                  (hash-set env class_tag loc))
                 (begin ;write the class into store
                  (hash-set! store loc 
                        (VObj not_in_store  'type not_in_store constructor))
                   constructor))))))))] 
   |#
        
        [CInstance (t l) 
                   ;(local 
                   ;([define found (lookup t (unbox menv))])
                   ;(type-case VAnswer found
                   ;[VResult (v) 
                   (VResult 
                    (make-bind-VObj t l 
                                    ;(string->symbol 
                                    ;(string-append "instance of" 
                                    ;(symbol->string t)))
                                    (VMembers t (hash (list)))))]
        ;[VReturn (v) (error 'interp "Syntax Error")]
        ;[VError (v) found]))
        ;[none () (error 'interp (string-append "Class not exist: " (symbol->string t)))])]
        ;interp the try and exception
        [CTryEx (t eh orelse)
                (local ([define v-answer (interp t)])
                  (type-case VAnswer v-answer
                    [VResult (v) (interp orelse)]
                    [VReturn (v) (error 'interp "Syntax Error15")];todo???
                    [VError (v) 
                            ;first bind the exception
                            (begin
                              ;(print-envs (unbox menv))
                              ;(display "\n")
                              ;(print-store)
                              ;(display "\n")
                              ;solution 1
                              ;(interp 
                              ;(CLet 'try-exception (CInstance (VObj-type v) not_in_store) eh)))]))] ;??
                              ;solution2
                              ;error is not a simple typle
                              (push-frame (hash-set env 'try-exception (VObj-id v)))
                              (local
                                ([define value (interp eh)])
                                (begin
                                  (pop-frame)
                                  value)))]))]
        [CFinally (t f)
                  (local ([define v-answer (interp t)])
                    (type-case VAnswer v-answer
                      [VResult (v) (interp f)]
                      [VReturn (v) (error 'interp "Syntax Error15")];todo???
                      [VError (v) 
                              (begin (interp f) (interp (CRaise (CInstance (VObj-type v) not_in_store))))]))]
        [CUndefined() 
                   (VResult
                    (make-bind-VObj 'undefined not_in_store
                                    (VNone)))] 
        [CList(b) 
              (VResult
               (make-bind-VObj 'list (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                               (VList (mt) b)))] 
        [CDictKeys (d)
                   (local 
                     ([define d-answer (interp d)])
                     (type-case VAnswer d-answer
                       [VReturn (vobj) (error 'interp "Syntax Errorad15")];todo???
                       [VError (vobj) d-answer] 
                       [VResult (vobj)
                                (if
                                 (VNone? (VObj-value vobj))
                                 (VError
                                  (make-bind-VObj 'TypeError not_in_store (VStr (string->list "None as arg"))))
                                 (get-dict-keys (VDict-d (VObj-value vobj))))]))]
        [CDictValues (d)
                     (local 
                       ([define d-answer (interp d)])
                       (type-case VAnswer d-answer
                         [VReturn (vobj) (error 'interp "Syntax Errorad15")];todo???
                         [VError (vobj) d-answer] 
                         [VResult (vobj)
                                  (if
                                   (VNone? (VObj-value vobj))
                                   (VError
                                    (make-bind-VObj 'TypeError not_in_store (VStr (string->list "None as arg"))))
                                   (get-dict-values (VDict-d (VObj-value vobj))))]))]
        [CDictItems (d)
                    (local 
                      ([define d-answer (interp d)])
                      (type-case VAnswer d-answer
                        [VReturn (vobj) (error 'interp "Syntax Errorad15")];todo???
                        [VError (vobj) d-answer] 
                        [VResult (vobj)
                                 (if
                                  (VNone? (VObj-value vobj))
                                  (VError
                                   (make-bind-VObj 'TypeError not_in_store (VStr (string->list "None as arg"))))
                                  (get-dict-items (VDict-d (VObj-value vobj))))]))]
        [CCons (v r) 
               (local 
                 ([define v-answer (interp v)]
                  [define r-answer (interp r)])
                 (type-case VAnswer v-answer
                   [VReturn (vobj) (error 'interp "Syntax Error15")];todo???
                   [VError (vobj) v-answer] 
                   [VResult (vobj)
                            (type-case VAnswer r-answer
                              [VReturn (robj) (error 'interp "Syntax Error15")];todo???
                              [VError (robj) r-answer] 
                              [VResult (robj) 
                                       (VResult
                                        (make-bind-VObj 'list (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                                        (VList (node vobj
                                                                     (VList-l (VObj-value robj))) 
                                                               (VList-m (VObj-value robj)))))])]))]
        [CIsInstance (i c)
                     (begin ;(print-envs (unbox menv))
                       ;(print-store)
                       (local 
                         ([define i-answer (interp i)]
                          [define c-answer (interp c)])
                         (type-case VAnswer i-answer
                           [VReturn (iobj) (error 'interp "Syntax Error15a")];todo???
                           [VError (iobj) i-answer] 
                           [VResult (iobj)
                                    (type-case VAnswer c-answer
                                      [VReturn (cobj) (error 'interp "Syntax Error15a")];todo???
                                      [VError (cobj) c-answer] 
                                      [VResult (cobj) 
                                               (if 
                                                (or
                                                 (equal? (VObj-class_loc iobj) (VObj-id cobj))
                                                 (isChild (VObj-class_loc iobj) (VObj-id cobj))
                                                 )
                                                (VResult
                                                 (make-bind-VObj 'bool (VObj-id (VResult-v 
                                                                                 (lookup 'bool (unbox menv))))
                                                                 (VBool #t)))
                                                (begin
                                                  ;(display (constr (list "!!" 
                                                  ;(symbol->string (VObj-type iobj))
                                                  ;(symbol->string (CId-x c))
                                                  ;(number->string (VObj-class_loc iobj)) 
                                                  ;(number->string (VObj-id cobj)))))
                                                  (VResult
                                                   (make-bind-VObj 'bool (VObj-id (VResult-v 
                                                                                   (lookup 'bool (unbox menv))))
                                                                   (VBool #f)))))])])))]
        [CSubscript(o i)
                   (local 
                     ([define o-answer (interp o)])
                     (type-case VAnswer o-answer
                       [VReturn (oobj) (error 'interp "Syntax Eqrror15a")];todo???
                       [VError (oobj) o-answer] 
                       [VResult (oobj)
                                (cond 
                                  [(VStr? (VObj-value oobj))
                                   (VResult
                                    (make-bind-VObj 'str (VObj-id (VResult-v (lookup 'str (unbox menv))))
                                                    (VStr 
                                                     (slice-string-api
                                                      (VStr-s (VObj-value oobj))
                                                      (interp (CSlice-s i))
                                                      (interp (CSlice-e i))
                                                      (interp (CSlice-step i))))))]                 
                                  [else
                                   (local 
                                     ([define i-answer (interp i)])
                                     (type-case VAnswer i-answer
                                       [VReturn (iobj) (error 'interp "Syntwqax Error15a")];todo???
                                       [VError (iobj) i-answer] 
                                       [VResult (iobj) (cond
                                                         [(and (VList? (VObj-value oobj)) 
                                                               (VNum? (VObj-value iobj)))
                                                          (VResult 
                                                           (list-subscript-get (VList-l (VObj-value oobj)) 
                                                                               (VNum-n (VObj-value iobj))))]
                                                         [(VDict? (VObj-value oobj))
                                                          (type-case (optionof VObjType) 
                                                            (hash-ref (VDict-d (VObj-value oobj))  iobj )
                                                            [some (v) 
                                                                  (VResult v)]
                                                            [none() (error 'Keyerror "key erros to mkae")])]
                                                         [else
                                                          (error 'interp "invalid asubscript")])]))])]))] 
        ;get the id
        [CGetObjId (o)  
                   (local ([define o-answer (interp o)])
                     (type-case VAnswer o-answer
                       [VReturn (v) (error 'interp "Syntax asdError15")];todo???
                       [VError (v) o-answer]
                       [VResult (v) (VResult (make-bind-VObj 'int 
                                                             (VObj-id (VResult-v (lookup 'int (unbox menv)))) 
                                                             (VNum (VObj-id v))))]))]
        ;get the class id
        [CGetClassId (o)
                     (local ([define o-answer (interp o)])
                       (type-case VAnswer o-answer
                         [VReturn (v) (error 'interp "Syntax asdError15")];todo???
                         [VError (v) o-answer]
                         [VResult (v) (VResult (make-bind-VObj 'int 
                                                               (VObj-class_loc (VResult-v 
                                                                                (lookup 'int (unbox menv)))) 
                                                               (VNum (VObj-id v))))]))]
        [CSet (values)
              (interp-set values)]
        
        [CDictGet (d  k df  )
                  (local ([define dic (interp d)]    
                          [define key (interp k)]
                          [define default (interp df)])
                    (cond
                      [(not (VDict? (VObj-value (VResult-v dic))))
                       (VError
                        (make-bind-VObj 'TypeError not_in_store (VStr (string->list "No Dict"))))]
                      [else
                       (type-case (optionof VObjType) 
                         (hash-ref (VDict-d (VObj-value 
                                             (VResult-v dic))) (VResult-v key))
                         [some (v) 
                               (VResult v)]
                         [none() default])]))]
        [CSlice(a b c ) (mkerr "should not interp this")]
        
        [CDictUpdate (x d)
                     (local ([define arg-tuple (VResult-v (interp d))]) 
                       (cond
                         [(mt? (VList-l (VObj-value arg-tuple)))
                          (interp x)] 
                         [(not (mt? (node-next (VList-l (VObj-value arg-tuple)))))
                          (VError
                           (make-bind-VObj 
                            'TypeError not_in_store 
                            (VStr 
                             (string->list "more values than dict update needed"))))]
                         [else 
                          (begin
                            ;(display "was")
                            ;(display (pretty-obj (VResult-v (interp x))))
                            ;(display "\n")
                            (hash-set! store
                                       (VObj-id (VResult-v (interp x)))
                                       (VObj
                                        (VObj-id (VResult-v (interp x)))
                                        'dict (VObj-id (VResult-v (lookup 'dict (unbox menv)))) 
                                        (VDict (update-dict (VDict-d (VObj-value (VResult-v (interp x))))
                                                            (VDict-d 
                                                             (VObj-value (node-value 
                                                                          (VList-l (VObj-value arg-tuple)))))))))
                            ;(display "now")
                            ;(display (pretty-obj (VResult-v (interp x))))
                            ;(display "\n")
                            (interp x))]))]
        ;[CGlobal (x) (local
        ;([define obj (make-bind-VObj 'undefined not_in_store (VNone))]
        ;[define newloc (VObj-id obj)]
        ;[define new-env (hash-set (hash (list)) x newloc)]
        ;)
        ;(begin
        ;(add-to-bottom new-env (unbox menv))
        ;(print-store)
        ;(VResult obj)))]
        [CNonlocal (x) 
                   (local
                     ([define loc (look-and-up-with-env x (rest (unbox menv)))])
                     (begin
                       (update-frame (hash-set env x loc))
                       (interp (CPass))))]
        ;[else (begin (printCExp expr) (error 'interp "dummy1" ))] 
        ))))


;; a shortcut of place holder
;;
(define (err)
  (mkerr "THis is a palce holder"))


;; add this env to bottom of env stacks
;;
(define (add-to-bottom [newEnv : Env] [stack : (listof Env)])
  (if(empty? stack)
     (set! menv (box (list newEnv)))
     (begin
       (add-to-bottom newEnv (rest stack))
       (set! menv (box (cons (first stack) (unbox menv)))))))


;; update dictionary
;;
(define (update-dict [x : (hashof VObjType VObjType)] [di : (hashof VObjType VObjType)]) : (hashof VObjType VObjType)
  (begin
    ;;(pln "get" di)
    ;(display "get")
    ;(display (pretty (VDict di)))
    ;(display "\n")
    (cond
      [(empty? (hash-keys di)) x]
      [else
       (local ([define this_key (first (hash-keys di))])
         (update-dict
          (hash-set x this_key
                    (some-v (hash-ref di this_key)))
          (hash-remove di this_key)))])))


;; get the keys of dict as set
;;
(define (get-dict-keys [d : (hashof VObjType VObjType)]) : VAnswer
  (local ([define key-list (hash-keys d)])
    (VResult 
     (make-bind-VObj 'set (VObj-id (VResult-v (lookup 'set (unbox menv))))
                     (VSet (keys-list-to-vset key-list))))))


;; get the items of dict as set
;;
(define (get-dict-items [d : (hashof VObjType VObjType)]) : VAnswer
  (local ([define key-list (hash-keys d)])
    (VResult 
     (make-bind-VObj 'set (VObj-id (VResult-v (lookup 'set (unbox menv))))
                     (VSet (keys-list-to-items-vset key-list d))))))


;; get the values of dict as set
;;
(define (get-dict-values [d : (hashof VObjType VObjType)]) : VAnswer
  (local ([define key-list (hash-keys d)])
    (VResult 
     (make-bind-VObj 'set (VObj-id (VResult-v (lookup 'set (unbox menv))))
                     (VSet (keys-list-to-values-vset key-list d))))))


;; make a VSet of hash keys
;;
(define (keys-list-to-vset (s : (listof VObjType))) : (hashof CVal VObjType)
  (cond
    [(empty? s) (hash empty)]
    [else
     (hash-set 
      (keys-list-to-vset (rest s))
      (VObj-value 
       (first s))
      (first s))]))


;; make a VSet of hash  items
;;
(define (keys-list-to-items-vset (s : (listof VObjType)) [d : (hashof VObjType VObjType)]) : (hashof CVal VObjType)
  (cond
    [(empty? s) (hash empty)]
    [else
     (local ([define list_u 
               (make-bind-VObj 'list
                               (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                               (VList
                                (node
                                 (first s)
                                 (node
                                  (some-v (hash-ref d (first s)))
                                  (mt)))
                                #f))])
       (hash-set 
        (keys-list-to-items-vset (rest s) d )
        (VObj-value 
         list_u) list_u ))]))


;; make a VSet of hash  values
;;
(define (keys-list-to-values-vset (s : (listof VObjType)) [d : (hashof VObjType VObjType)]) : (hashof CVal VObjType)
  (cond
    [(empty? s) (hash empty)]
    [else
     (local ([define v (some-v (hash-ref d (first s)))])
       (hash-set 
        (keys-list-to-items-vset (rest s) d )
        (VObj-value 
         v) v ))]))


;; interp a Cset
;;
(define (interp-set [s : (listof CExp)]) : VAnswer
  (cond 
    [(empty? s) 
     (VResult (make-bind-VObj 'set 
                              (VObj-id (VResult-v (lookup 'set (unbox menv)))) (VSet (hash empty))))]
    [else 
     (local ([define answer (interp (first s))])
       (type-case VAnswer answer
         [VReturn (v) (error 'interp "Syntax asdErrsdsor15")];todo???
         [VError (v) answer]
         [VResult (v) 
                  (VResult (make-bind-VObj 'set 
                                           (VObj-id 
                                            (VResult-v 
                                             (lookup 'set (unbox menv))))
                                           (VSet
                                            (hash-set 
                                             (VSet-s (VObj-value (VResult-v 
                                                                  (interp-set (rest s))))) 
                                             (VObj-value v) v))))]))]))


;; System.out.println
;;
(define (pln [des : string] a) : void
  (begin
    (display des)
    (display " : ")
    (display a)
    (display "\n")))


;; slice string api, providing a api to slice string
;;
(define (slice-string-api [str : (listof char)]
                          [start-a : VAnswer] [end-a : VAnswer] 
                          [step-a : VAnswer]) : (listof char)
  ;to check Verrors
  (local ([define start (VObj-value (VResult-v start-a))]
          [define end (VObj-value (VResult-v end-a))]
          [define step (VObj-value (VResult-v step-a))])
    (begin
      ;(pln "str" str)
      ;(pln "start" start)
      ;(pln "end" end)
      ;(pln "step" step)
      ;check  step exists
      (if
       (VNone? step)
       (set! step (VNum 1))
       (set! step step))
      ;check start
      (if
       (VNone? start)
       (if (< (VNum-n step) 0)
           ;(set! start (VNum (length str)))
           (set! start (VNum 0))
           (set! start (VNum 0)))   
       (if (< (VNum-n step) 0)
           (set! start (VNum (- (- (length str) (VNum-n start)) 1)))
           (set! start start )))
      
      ;check end
      (if
       (VNone? end)
       (if (< (VNum-n step) 0)
           ;(set! end (VNum 0))
           (set! end (VNum (length str)))
           (set! end (VNum (length str))))   
       
       (if (< (VNum-n step) 0)
           (set! end (VNum (- (length str) (VNum-n end))))
           (set! end end )))
      
      (slice-string
       (if 
        (< (VNum-n step) 0)
        (reverse str)
        str)
       (VNum-n start)
       (VNum-n end)
       (if 
        (< (VNum-n step) 0)
        (* -1 (VNum-n step))
        (VNum-n step))))))


;; string slicing  function
;;
(define (slice-string [str : (listof char)] [start : number] [end : number] [step : number]) : (listof char)
  (begin
    ;(pln "str" str)
    ;(pln "start" start)
    ;(pln "end" end)
    ;(pln "step" step)
    (cond
      [(< start 0)
       (slice-string str 0 end step)]
      ;[(< step 0)
      ;(slice-string (reverse str) (- (length str) start) (- (length str) end) (* -1 step))]
      [else
       (select-string-from-zero (trim-string-to-slice-start str start) end step 0)]))) 


;; trim string to particular char
;;
(define (trim-string-to-slice-start [str : (listof char)] [start : number]) : (listof char)
  (cond
    [(equal? start 0) 
     str]
    [(> start 0)
     (trim-string-to-slice-start (rest str) (- start 1))]
    [else
     (mkerr "wrong")]))



;; slice string from start
;;
(define (select-string-from-zero [str : (listof char)] 
                                 [end : number] 
                                 [step : number] 
                                 [current : number]) : (listof char)  
  (cond
    [(empty? str) empty]
    [(equal? end 0) empty ]
    [(equal? current 0)
     (cons (first str) 
           (select-string-from-zero (rest str)
                                    (- end 1)
                                    step
                                    (if
                                     (< current (- step 1))
                                     (+ current 1)
                                     0)))]
    [else
     (select-string-from-zero (rest str)
                              (- end 1)
                              step
                              (if
                               (< current (- step 1))
                               (+ current 1)
                               0)
                              )]))


;; get item out of resursive list
;;
(define (list-subscript-get [l : ListVal] [index : number]) : VObjType
  (cond
    [(and (not (mt? l)) (equal? index 0)) (node-value l)] 
    [(mt? l) (error 'interp "index out of bound : list")]
    [else
     (list-subscript-get (node-next l) (- index 1))])) 




#|
;helpe method : bind undefined into the current env and store
;we assume the location -1 is undefined 

;;
;;
(define (add-undefined-to-store ) : void
 (hash-set! store -1 (make-bind-VObj 'undefined (VNone))))
|#

;help method to bind all the hosted variables

;; bind hoisted variables to env
;;
(define (bind-hoisted-variables [vs : (listof symbol)]) : void
  (cond 
    [(not (empty? vs)) ;;add case for no hoisted variables
     (begin
       (local ([define loc (next-loc)])
         (begin (update-frame (hash-set (first (unbox menv)) (first vs) loc))
                (hash-set! store loc (make-bind-VObj 'undefined not_in_store (VNone)))))
       (bind-hoisted-variables (rest vs)))]
    [else (void)]))



;; look up from identifier to answer
;;
(define (lookup [x : symbol] [envs : (listof Env)]) : VAnswer
  
  (local ([define the_loc (look-and-up-with-env x envs)]
          [define error_str 
            (if
             (equal? x 'try-exception)
             (VStr (string->list "No active exception"))
             
             (VStr (string->list 
                    (constr (list "No " (number->string the_loc) " in Store: " (symbol->string x))))))])
    
    (cond
      [(equal? not_in_env the_loc) 
       (VError (make-bind-VObj 'RuntimeError 
                               (VObj-id (VResult-v (lookup 'RuntimeError (unbox menv)))) 
                               error_str))] 
      
      [else 
       
       (type-case (optionof VObjType) (hash-ref store the_loc )
         [some (v) 
               (cond
                 [(symbol=? (VObj-type v) 'undefined)
                  (VError (make-bind-VObj 'RuntimeError 
                                          (VObj-id (VResult-v (lookup 'RuntimeError (unbox menv)))) 
                                          error_str))] 
                 [else (VResult v)])]
         [none();(error 'interp (string-append "Undefined identifier : " (symbol->string x)))]))
              (VError (make-bind-VObj 'RuntimeError 
                                      (VObj-id (VResult-v (lookup 'RuntimeError (unbox menv)))) 
                                      error_str))])])))  

;;;look up through levels of environments
;;;with the current environment
;(;define (look-and-up x) : Location
;(look-and-up-with-env x (unbox menv)))


;; look up through levels of environments
;;
(define (look-and-up-with-env [x : symbol] [envs : (listof Env)]) : Location
  (cond 
    [(empty? envs) not_in_env];-2 
    [else (type-case (optionof Location) (hash-ref (first envs) x)
            [some (l) l]
            [none() (look-and-up-with-env x (rest envs))])]))


;; look and up, with default env
;;
(define (look-and-up [x : symbol]) : Location
  (look-and-up-with-env x (unbox menv)))




;; not in env alias
;;
(define not_in_env -2)


;; interp the dictionary 
;;
(define (interp-CDict [keys : (listof CExp)] 
                      [values : (listof CExp)] 
                      [h : (hashof VObjType VObjType)]) : VAnswer
  (cond
    [(empty? keys) (VResult (make-bind-VObj 'dict (VObj-id (VResult-v (lookup 'dict (unbox menv)))) (VDict h)))]
    [else
     (local ([define k-answer (interp (first keys))])
       (type-case VAnswer k-answer
         [VReturn (k) (error 'interp-CDict "Syntax Error17")]
         [VError (k) k-answer]
         [VResult (k) 
                  (local ([define v-answer (interp (first values))])
                    (type-case VAnswer v-answer
                      [VReturn (v) (error 'interp-CDict "Syntax Error18")]
                      [VError (v) v-answer]
                      [VResult (v) 
                               (interp-CDict (rest keys) (rest values) (hash-set h k v))]))]))]))



;; interp comparision
;;
(define (interp-CCompare [op : symbol] [left : CExp] [right : CExp]) : VAnswer
  (local ([define l-answer (interp left)]
          [define r-answer (interp right)])
    (type-case VAnswer l-answer
      [VReturn (v) (error 'interp "Syntax Error19")]
      [VError (v) l-answer]
      [VResult (l-obj) 
               (type-case VAnswer r-answer
                 [VReturn (v) (error 'interp "Syntax Error19")]
                 [VError (v) r-answer]
                 [VResult (r-obj)
                          (interp-CCompare-aux op l-obj r-obj)])])))



;; interp comparision helper
;;
(define (interp-CCompare-aux [op : symbol] [l-obj : VObjType] [r-obj : VObjType]) : VAnswer
  (cond
    [(symbol=? 'is op) 
     (cond
       [(or 
         (equal? (VObj-type l-obj) 'int)
         (equal? (VObj-type l-obj) 'bool)
         (equal? (VObj-type l-obj) 'str)
         (equal? (VObj-type l-obj) 'float))
        (begin
          ;(display "Got here2\n")
          ;(display l-obj)
          ;(display "Got here2\n")
          ;(display "Got here1")
          (interp-CCompare-aux 'eq l-obj r-obj)
          )]
       
       
       [else
        (begin
          ;(display "Got here3\n")
          ;(display "Got here1")
          (VResult (make-bind-VObj 'bool 
                                   (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                   (VBool (= (VObj-id l-obj) (VObj-id r-obj))))))])]
    
    [(symbol=? 'isnot op) 
     (cond
       [(or 
         (equal? (VObj-type l-obj) 'int)
         (equal? (VObj-type l-obj) 'bool)
         (equal? (VObj-type l-obj) 'str)
         (equal? (VObj-type l-obj) 'float))
        (interp-CCompare-aux 'noteq l-obj r-obj)]
       
       
       [else
        (begin
          ;;(display l-obj)
          ;(display "\n")
          ;(display r-obj)
          ;(display "\n")
          (VResult (make-bind-VObj 'bool 
                                   (VObj-id (VResult-v
                                             (lookup 'bool (unbox menv)))) 
                                   (VBool (not (= (VObj-id l-obj) (VObj-id r-obj)))))))])]
    
    
    
    
    [(symbol=? 'in op) 
     
     (local ([define lstr (string->list (make-string-based-on-value (VObj-value l-obj)))]
             [define rstr (string->list (make-string-based-on-value (VObj-value r-obj)))]
             )
       
       (begin 
         ;(display lstr)
         ;(display "\n")
         ;(display rstr)
         ;(display "\n")
         (VResult (make-bind-VObj 'bool 
                                  (VObj-id (VResult-v
                                            (lookup 'bool (unbox menv)))) 
                                  (VBool (isSubString lstr rstr))))))]
    
    
    [(symbol=? 'notin op) 
     
     (local ([define lstr (string->list (make-string-based-on-value (VObj-value l-obj)))]
             [define rstr (string->list (make-string-based-on-value (VObj-value r-obj)))]
             )
       
       (begin 
         ;(display lstr)
         ;(display "\n")
         ;(display rstr)
         ;(display "\n")
         (VResult (make-bind-VObj 'bool 
                                  (VObj-id (VResult-v
                                            (lookup 'bool (unbox menv)))) 
                                  (VBool (not (isSubString lstr rstr)))))))]
    
    
    
    [else
     (cond 
       [(VNum? (VObj-value l-obj))
        (cond 
          [(symbol=? 'noteq op)
           
           (cond
             [(VNum? (VObj-value r-obj))
              (VResult
               (make-bind-VObj 'bool 
                               (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                               (VBool (not (= (VNum-n (VObj-value l-obj)) 
                                              (VNum-n (VObj-value r-obj)))))))]
             [else
              (VResult (make-bind-VObj 'bool 
                                       (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                       (VBool  #t )))])]  
          
          [(symbol=? 'eq op)
           
           (cond
             [(VNum? (VObj-value r-obj))
              (VResult
               (make-bind-VObj 'bool 
                               (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                               (VBool (= (VNum-n (VObj-value l-obj)) 
                                         (VNum-n (VObj-value r-obj))))))]
             [else
              (VResult (make-bind-VObj 'bool 
                                       (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                       (VBool  #f )))])] 
          
          
          [(symbol=? 'lt op)
           (VResult 
            (make-bind-VObj 'bool 
                            (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                            (VBool (< (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj))))))]
          [(symbol=? 'gt op)
           (VResult 
            (make-bind-VObj 'bool 
                            (VObj-id (VResult-v (lookup 'bool (unbox menv))))
                            (VBool (> (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj))))))]
          [(symbol=? 'lte op)
           (VResult 
            (make-bind-VObj 'bool 
                            (VObj-id (VResult-v (lookup 'bool (unbox menv))))
                            (VBool (<= (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj))))))]
          [(symbol=? 'gte op)
           (VResult 
            (make-bind-VObj 'bool 
                            (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                            (VBool (>= (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj))))))]
          [else (error 'interp "not taken care of 1")])]
       
       [(VBool? (VObj-value l-obj))
        (cond 
          [(symbol=? 'noteq op)
           (VResult 
            (make-bind-VObj 'bool 
                            (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                            (VBool (not (equal? (VBool-b (VObj-value l-obj))
                                                (VBool-b (VObj-value r-obj)))))))]
          [(symbol=? 'eq op)
           (begin
             ;(display "Got here1\n")
             ;(display "Got here1")
             (VResult (make-bind-VObj 'bool 
                                      (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                      (VBool (equal? (VBool-b (VObj-value l-obj)) 
                                                     (VBool-b (VObj-value r-obj))))))
             )
           ]
          [else (error 'interp "not taken care of 22")])]
       
       
       [(VStr? (VObj-value l-obj))
        (cond 
          [(symbol=? 'noteq op)
           (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                    (VBool (not (equal? (list->string (VStr-s (VObj-value l-obj)))
                                                        (list->string (VStr-s (VObj-value r-obj))))))))]
          [(symbol=? 'eq op)
           (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                    (VBool (equal? (list->string (VStr-s (VObj-value l-obj))) 
                                                   (list->string (VStr-s (VObj-value r-obj)))))))]
          [(symbol=? 'lt op) 
           (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                    (VBool (string<? (list->string (VStr-s (VObj-value l-obj))) 
                                                     (list->string (VStr-s (VObj-value r-obj)))))))]
          
          [(symbol=? 'lte op) 
           (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                    (VBool (string<=? (list->string (VStr-s (VObj-value l-obj))) 
                                                      (list->string (VStr-s (VObj-value r-obj)))))))]
          
          
          [(symbol=? 'gt op) 
           (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                    (VBool (string>? (list->string (VStr-s (VObj-value l-obj))) 
                                                     (list->string (VStr-s (VObj-value r-obj)))))))]
          
          [(symbol=? 'gte op) 
           (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                    (VBool (string>=? (list->string (VStr-s (VObj-value l-obj))) 
                                                      (list->string (VStr-s (VObj-value r-obj)))))))]
          
          
          
          [else (error 'interp "not taken care of 22")])]
       
       
       
       
       [(VNone? (VObj-value l-obj))
        (cond 
          [(symbol=? 'eq op)
           (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                    (VBool (VNone? (VObj-value r-obj)))))] 
          
          [(symbol=? 'noteq op)
           (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) 
                                    (VBool (not (VNone? (VObj-value r-obj))))))]
          [else (error 'interp "not taken care of 22")])]
       
       
       [(VList? (VObj-value l-obj))
        (compare-VList op l-obj r-obj)]
       
       
       [(VDict? (VObj-value l-obj))
        (compare-VDict op l-obj r-obj)]
       
       [(VSet? (VObj-value l-obj))
        
        (VResult
         (make-bind-VObj 
          'bool not_in_store 
          (VBool
           (compare-set op 
                            
                            (VSet-s (VObj-value (remove-loc-info l-obj)))
                            (VSet-s (VObj-value (remove-loc-info r-obj)))))))]
       
       
       [else
        (begin (display (VObj-value l-obj))
               (begin (display (VObj-value r-obj))
                      (error 'interp "not taken care of 333")))] 
       
       )]))





;; compare two v list
;;
(define (compare-VList [op : symbol] [l-obj : VObjType] [r-obj : VObjType]) : VAnswer
  (cond
    [(symbol=? op 'eq);TODO: we can chekc type here !
     (local ([define ll (VList-l (VObj-value l-obj))] 
             [define rl (VList-l (VObj-value r-obj))])
       (compare-VList-aux op ll rl))]
    
    [(symbol=? op 'noteq);TODO: we can chekc type here !
     (VResult
      (make-bind-VObj 
       'bool not_in_store 
       (VBool
        (not (VBool-b (VObj-value 
                       (VResult-v 
                        (compare-VList 'eq l-obj r-obj))))))))] 
    
    [else
     (error 'compare "huh?")]))



;; compare list helper
;;
(define (compare-VList-aux [op : symbol] [ll : ListVal] [rl : ListVal]) : VAnswer
  (cond 
    [(and (mt? ll) (mt? rl))
     (VResult (make-bind-VObj 'bool 
                              (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #t)))]
    [(and (mt? ll) (not (mt? rl))) 
     (VResult 
      (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #f)))]
    [(and (mt? rl) (not (mt? ll)))
     (VResult (make-bind-VObj 'bool 
                              (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #f)))]
    [(equal? #f
             (VBool-b (VObj-value (VResult-v 
                                   (interp-CCompare-aux op (node-value ll) (node-value rl))))))
     (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #f)))]
    [else
     (compare-VList-aux op (node-next ll) (node-next rl))]))



 
;; compare two dictinary
;;
(define (compare-VDict [op : symbol] [l-obj : VObjType] [r-obj : VObjType]) : VAnswer
  (cond
    [(equal? op 'eq)
     (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv))))
                              (VBool
                               (local ([define eq (equal?
                                                   (begin
                                                     ;(display "\n11\n")
                                                     ;(display 
                                                     ;(pretty-obj (remove-loc-info l-obj)))
                                                     (pretty-obj (remove-loc-info l-obj)))
                                                   ;(pretty-obj-no-loc  l-obj))
                                                   
                                                   (begin
                                                     ;(display "\n22\n")
                                                     ;(display 
                                                     ;(pretty-obj (remove-loc-info r-obj)))
                                                     ;(display "\n33\n")
                                                     (pretty-obj (remove-loc-info r-obj))
                                                     ))])
                                 (begin
                                   ;(pln "result :" eq)
                                    eq))
                               ;(pretty-obj-no-loc  r-obj))
                               )))]
    [else
     (error 'interp "not done")]))




;; compare two sets
;;
(define (compare-set [op : symbol] [d1 : (hashof CVal VObjType)] [d2 : (hashof CVal VObjType)]) : boolean
  (cond
    [(equal? op 'eq)
     (set-equal? d1 d2)]
    [else
     (err)]))



;; if two sets are equal
;;
(define (set-equal? [d1 : (hashof CVal VObjType)] [d2 : (hashof CVal VObjType)]) : boolean
  
  (begin
    ;(display "\n")
    ;(display d1)
    
    ;(display "\n")
    ;(display d2)
    
    ;(display "\n")
    (cond
      [(and 
        (empty? (hash-keys d1)) 
        (empty? (hash-keys d2))) #t]
      [
       else
       (local 
         ([define this_key (first (hash-keys d2))])
         (type-case (optionof VObjType) (hash-ref d1 this_key)
           [some (vd1)  
                 (if     
                  (VBool-b (VObj-value (VResult-v(interp-CCompare-aux 
                                                  'eq 
                                                  (remove-loc-info (some-v (hash-ref d1 this_key )))
                                                  (remove-loc-info (some-v (hash-ref d2 this_key )))))))
                  (set-equal?
                   (hash-remove d1 this_key)
                   (hash-remove d2 this_key))
                  
                  #f)]
           [none () #f]))])))

 
;; interp CUnary operations
;;
(define (interp-CUnaryOp [op : symbol] [operand : CExp]) : VAnswer
  (local ([define operand-answer (interp operand)])
    (type-case VAnswer operand-answer
      [VReturn (v) (error 'interp "Syntax Error20")]
      [VError (v) operand-answer]
      [VResult (operand-obj) 
               (cond 
                 [(symbol=? op 'not)
                  (VResult 
                   (make-bind-VObj 'bool
                                   (VObj-id (VResult-v (lookup 'bool (unbox menv))))
                                   (VBool (not (isTrue operand-obj)))))]
                 [(symbol=? op 'usub)
                  (VResult 
                   (make-bind-VObj 'int 
                                   (VObj-id (VResult-v (lookup 'int (unbox menv))))
                                   (VNum (* -1 (VNum-n (VObj-value operand-obj))))))]
                 [(symbol=? op 'invert)
                  (VResult
                   (make-bind-VObj 'int 
                                   (VObj-id (VResult-v (lookup 'int (unbox menv)))) 
                                   (VNum (- (* -1 (VNum-n (VObj-value operand-obj))) 1))))]
                 
                 
                 ;[(symbol=? op 'uadd)
                 
                 ;(lookup '__pos__ 
                 ;(VClosure-member (VObj-value (look-up (VObj-type operand-obj) (unbox menv)))))
                 
                 ;]
                 [else (error 'interp "not taken care of 4")])])))



;; interp Bool Op
;;
(define (interp-CBoolOp (op : symbol) (l : CExp) (r : CExp)) : VAnswer
  (local ([define l-answer (interp l)]
          [define r-answer (interp r)])
    (type-case VAnswer l-answer
      [VReturn (v) (error 'interp "Syntax Error21")]
      [VError (v) l-answer]
      [VResult (l-obj) 
               (type-case VAnswer r-answer
                 [VReturn (v) (error 'interp "Syntax Error22")]
                 [VError (v) r-answer]
                 [VResult (r-obj) 
                          (cond 
                            [(symbol=? op 'or) 
                             (cond
                               [(isTrue l-obj) l-answer]
                               [else r-answer])]
                            [(symbol=? op 'and) 
                             (cond
                               [(isTrue (VResult-v l-answer)) r-answer]
                               [else l-answer])]
                            [else (error 'interp "not taken care of 2")]
                            )])])))



;; interp CBinOP
;;
(define (interp-CBinOp (op : symbol) (l : CExp) (r : CExp)) : VAnswer
  (local ([define l-answer (interp l)]
          [define r-answer (interp r)])
    (type-case VAnswer l-answer
      [VReturn (v) (error 'interp "Syntax Error23")]
      [VError (v) l-answer]
      [VResult (v) 
               (type-case VAnswer r-answer
                 [VReturn (v) (error 'interp "Syntax Error24")]
                 [VError (v) r-answer]
                 [VResult (v) 
                          (cond 
                            [(and
                              (or
                               (VNum? (VObj-value (VResult-v l-answer)))
                               (VBool? (VObj-value (VResult-v l-answer))))
                              
                              (or
                               (VNum? (VObj-value (VResult-v r-answer)))
                               (VBool? (VObj-value (VResult-v r-answer)))))
                             (begin
                               (set! l-answer (interp (CPrim1 'cast-int l)))
                               
                               (set! r-answer (interp (CPrim1 'cast-int r)))
                               
                               (cond 
                                 [(symbol=? op 'add);TODO type checks and more 
                                  (VResult 
                                   (make-bind-VObj 'int 
                                                   (VObj-id (VResult-v (lookup 'int (unbox menv)))) 
                                                   (VNum (+ (VNum-n (VObj-value (VResult-v l-answer))) 
                                                            (VNum-n (VObj-value (VResult-v r-answer)))))))]
                                 [(symbol=? op 'sub);TODO type checks and more 
                                  (VResult 
                                   (make-bind-VObj 'int 
                                                   (VObj-id (VResult-v (lookup 'int (unbox menv)))) 
                                                   (VNum (- (VNum-n (VObj-value (VResult-v l-answer))) 
                                                            (VNum-n (VObj-value (VResult-v r-answer)))))))]
                                 [(symbol=? op 'mult);TODO type checks and more 
                                  (VResult 
                                   (make-bind-VObj 'int 
                                                   (VObj-id (VResult-v (lookup 'int (unbox menv)))) 
                                                   (VNum (* (VNum-n (VObj-value (VResult-v l-answer))) 
                                                            (VNum-n (VObj-value (VResult-v r-answer)))))))]
                                 [(symbol=? op 'div);TODO type checks and more 
                                  (if
                                   (= 0 (VNum-n (VObj-value (VResult-v r-answer))))
                                   (VError
                                    (make-bind-VObj 'ZeroDivisionError not_in_store (VStr (string->list "/0"))))
                                   (VResult 
                                    (make-bind-VObj 'int 
                                                    (VObj-id 
                                                     (VResult-v (lookup 'int (unbox menv)))) 
                                                    (VNum (/ (VNum-n (VObj-value (VResult-v l-answer))) 
                                                             (VNum-n (VObj-value (VResult-v r-answer))))))))]
                                 [(symbol=? op 'mod);TODO type checks and more 
                                  (if
                                   (= 0 (VNum-n (VObj-value (VResult-v r-answer))))
                                   (VError
                                    (make-bind-VObj 'ZeroDivisionError not_in_store (VStr (string->list "/0"))))
                                   (mkerr "humm"))]
                                 [(symbol=? op 'floordiv);TODO type checks and more 
                                  (if
                                   (= 0 (VNum-n (VObj-value (VResult-v r-answer))))
                                   (VError
                                    (make-bind-VObj 'ZeroDivisionError not_in_store (VStr (string->list "/0"))))
                                   (VResult
                                    (make-bind-VObj 'int 
                                                    (VObj-id (VResult-v 
                                                              (lookup 'int (unbox menv)))) 
                                                    (VNum (floor (/ (VNum-n 
                                                                     (VObj-value (VResult-v l-answer))) 
                                                                    (VNum-n (VObj-value (VResult-v r-answer)))))))))]
                                 
                                 [else (error 'interp "not taken care of 3")]))]
                            
                            [(or
                              (VStr? (VObj-value (VResult-v l-answer)))
                              (VStr? (VObj-value (VResult-v r-answer))))
                             
                             (cond 
                               [(symbol=? op 'add);TODO type checks and more 
                                (VResult 
                                 (make-bind-VObj 
                                  'str 
                                  (VObj-id 
                                   (VResult-v (lookup 'str (unbox menv))))
                                  (VStr (string->list
                                         (constr (list 
                                                  (list->string (VStr-s (VObj-value (VResult-v l-answer))))
                                                  (list->string 
                                                   (VStr-s
                                                    (VObj-value 
                                                     (VResult-v (interp (CPrim1 'cast-str r))))))))))))]
                               [(symbol=? op 'mult)
                                (VResult 
                                 (make-bind-VObj 
                                  'str 
                                  (VObj-id 
                                   (VResult-v (lookup 'str (unbox menv))))
                                  (VStr (string->list 
                                         (cond
                                           [(VStr? (VObj-value (VResult-v l-answer)))
                                            (string-mul 
                                             (list->string (VStr-s (VObj-value (VResult-v l-answer))))
                                             (VNum-n (VObj-value (VResult-v r-answer))))]
                                           [else
                                            (string-mul 
                                             (list->string (VStr-s (VObj-value (VResult-v r-answer))))
                                             (VNum-n (VObj-value (VResult-v l-answer))))])
                                         
                                         ))))]    
                               [else (error 'interp "not taken care of 3")])]
                            [(and
                              (VList? (VObj-value (VResult-v l-answer)))
                              ;(VNum? (VObj-value (VResult-v r-answer))))
                              #t)
                             
                             (cond
                               [(equal? op 'mult)
                                (VResult 
                                 (make-bind-VObj 
                                  'list 
                                  (VObj-id 
                                   (VResult-v (lookup 'list (unbox menv))))
                                  (VList 
                                   
                                   (list-mul  (VList-l (VObj-value (VResult-v l-answer)))
                                              (VNum-n (VObj-value (VResult-v r-answer))))
                                   (VList-m (VObj-value (VResult-v l-answer))))))]
                               
                               
                               
                               [(equal? op 'add)
                                (VResult 
                                 (make-bind-VObj 
                                  'list 
                                  (VObj-id 
                                   (VResult-v (lookup 'list (unbox menv))))
                                  (VList 
                                   
                                   (list-add  (VList-l (VObj-value (VResult-v l-answer)))
                                              (VList-l (VObj-value (VResult-v r-answer))))
                                   (VList-m (VObj-value (VResult-v l-answer))))))]
                               
                               )
                             
                             ]
                            
                            [(and
                              (VSet? (VObj-value (VResult-v l-answer)))
                              (VSet? (VObj-value (VResult-v r-answer))))
                             (cond
                               [(symbol=? op 'sub)
                                
                                (VResult (make-bind-VObj 'set
                                                         (VObj-id (VResult-v (lookup 'set (unbox menv)))) 
                                                         (VSet
                                                          (hash-sub (VSet-s (VObj-value (VResult-v l-answer)))
                                                                    (VSet-s (VObj-value (VResult-v r-answer)))))))
                                
                                
                                
                                ]
                               
                               
                               [(symbol=? op 'bitand)
                                
                                (VResult  
                                 (make-bind-VObj 'set
                                                 (VObj-id (VResult-v (lookup 'set (unbox menv)))) 
                                                 (VSet
                                                  (hash-inter (VSet-s (VObj-value (VResult-v l-answer)))
                                                              (VSet-s (VObj-value (VResult-v r-answer)))))))]
                               
                               
                               [(symbol=? op 'bitor)
                                
                                (VResult  
                                 (make-bind-VObj 'set
                                                 (VObj-id (VResult-v (lookup 'set (unbox menv)))) 
                                                 (VSet
                                                  (hash-union (VSet-s (VObj-value (VResult-v l-answer)))
                                                              (VSet-s (VObj-value (VResult-v r-answer)))))))]
                               
                               [(symbol=? op 'bitxor)
                                
                                (VResult  
                                 (make-bind-VObj 'set
                                                 (VObj-id (VResult-v (lookup 'set (unbox menv)))) 
                                                 (VSet
                                                  (hash-xor (VSet-s (VObj-value (VResult-v l-answer)))
                                                            (VSet-s (VObj-value (VResult-v r-answer)))))))]
                               
                               
                               [else (error 'interp "not taken care of 32")])
                             
                             ]
                            
                            
                            [else (error 'interp "not taken care of 32")])])])))


;set subtraction 
;(;define (hash-sub [d : (hashof CVal VObjType)] [ to-sub : (hashof CVal VObjType)]) : (hashof CVal VObjType)
;(
;
;hash-sub-aux d
;
;(hash-keys to-sub)
;
;))

;(;define (hash-sub-aux [d : (hashof CVal VObjType)] [keys : (listof CVal )]) : (hashof CVal VObjType)
;(cond
;[(empty? keys) d]
;[else
;(hash-remove (hash-sub-aux d (rest keys)) (first keys))]))

;set intersection
;(;define (hash-inter [d1 : (hashof CVal VObjType)] [d2 : (hashof CVal VObjType)]) : (hashof CVal VObjType)
;(
;;(mkerr "adasd") 
;hash-sub d1 (hash-sub d1 d2))
;;
;) 


;set union 
;(;define (hash-union [d : (hashof CVal VObjType)] [ to-sub : (hashof CVal VObjType)]) : (hashof CVal VObjType)
;(
;
;hash-uinion-aux d
;
;(hash-keys to-sub) to-sub
;
;))


;; set xor 
;;
(define (hash-xor [d1 : (hashof CVal VObjType)] [ d2 : (hashof CVal VObjType)]) : (hashof CVal VObjType)
  (hash-sub 
   (hash-union d1 d2)
   (hash-inter d1 d2))
  ) 

;(;define (hash-uinion-aux [d : (hashof CVal VObjType)] 
;[keys : (listof CVal )] [to-add : (hashof CVal VObjType)]) : (hashof CVal VObjType)
;(cond
;[(empty? keys) d]
;[else
;(hash-set (hash-uinion-aux d (rest keys) to-add) (first keys)
;(some-v (hash-ref to-add (first keys)))
;)]))

;; string multiplication
;;
(define (string-mul [s : string] [n : number]) : string
  (cond
    [(equal? 0 n) ""]
    [else
     (constr (list s (string-mul s (- n 1))))])) 


;; list multiplication
;;
(define (list-mul [s : ListVal] [n : number]) : ListVal
  (cond
    [(equal? 0 n) (mt)]
    [(equal? 1 n) s]
    [else
     (list-add s (list-mul s (- n 1)))]))


;; list add
;;
(define (list-add [a : ListVal] [v : ListVal]) : ListVal
  (cond
    [(mt? a) v]
    [
     else
     (node (node-value a) (list-add (node-next a) v))]))


;; if one object is True
;;
(define (isTrue (object : VObjType)) : boolean
  (begin
    ;(display "isTrue")
    ;(print object)
    (cond
      [(symbol=? 'str (VObj-type object))
       (cond 
         [(empty? (VStr-s (VObj-value object))) #f]
         [else #t])]
      [(symbol=? 'bool (VObj-type object))
       (VBool-b (VObj-value object))]
      [(symbol=? 'int (VObj-type object))
       (cond 
         [(= 0 (VNum-n (VObj-value object))) #f]
         [else #t])]
      [(symbol=? 'NoneType (VObj-type object)) #f]
      
      [(symbol=? 'dict (VObj-type object)) 
       (if 
        (empty? (hash-keys (VDict-d (VObj-value object))))
        #f
        #t)]
      
      [else #t])))



#|

;;
;;
(define (bind-args [args : (listof symbol)] [vals : (listof VObjType)] [env : Env]) : Env
 (cond [(and (empty? args) (empty? vals)) env]
    [(or (empty? args) (empty? vals))
    (begin
     (display (length args))
     (display (length vals))
     (map display (map symbol->string args))
     (error 'interp "Arity mismatch1"))]
    
    
    [(and (cons? args) (cons? vals))
    (local
     ([define loc (next-loc)])
     (begin
      (hash-set! store loc (first vals)) 
      (hash-set (bind-args (rest args) (rest vals) env)
           (first args) loc)))]
    [else (error 'interp "not taken care of 5")]
    ))
|#


; An Env, or An Verror
; 
(define-type EnvOrError
  [isEnv (e : Env)]
  [isErr (e : VAnswer)])



;; bind the interpreted list of values to the list of args in the given environment
;;
(define (bind-args [args : (listof symbol)] [vals : (listof VObjType)] [env : Env]) : EnvOrError
  (begin 
    ;(display "args binds \n")
    ;(display args)
    
    ;(display vals)
    ;(display "\n")
    (cond
      ;takes no arguments
      [(empty? args)
       (cond
         [(empty? vals) (isEnv env)];no aruguments
         [else 
          (begin
            
            ;(display "!!")
            ;(display args)
            ;(pln "args:" (map pretty-obj vals))
            ;(display "\n")
            (isErr (VError
                    (make-bind-VObj 
                     'TypeError not_in_store 
                     (VStr (string->list "Arity mismatch: extra values , 0 args"))))))])]
      
      ;takes arguments
      [else
       (local ([define arg (first args)])
         (cond 
           [(equal? arg '--tuple-star--) ;the rest are tuples
            (local
              ([define loc (next-loc)])
              (begin
                (hash-set! store loc (make-tuple vals)) 
                (isEnv (hash-set env
                                 (first (rest args)) loc))))]
           
           [(equal? arg '--default-none--) ;followed argue has a defualt value
            (cond
              [(empty? vals)
               (type-case EnvOrError (bind-args (rest (rest args)) vals env)
                 [isEnv(ise)
                       (isEnv (hash-set ise (first (rest args)) (look-and-up 'None)))]
                 [isErr(ise)
                       (isErr ise)])]
              [else 
               (local
                 ([define loc (next-loc)])
                 (begin
                   (if
                    (isSimpleType (VObj-type (first vals)))
                    (hash-set! store loc (first vals)) 
                    (set! loc (VObj-id (first vals))))
                   #|
         (hash-set (bind-args (rest args) (rest vals) env)
              (first args) loc)
          |#
                   (type-case EnvOrError (bind-args (rest (rest args)) (rest vals) env)
                     [isEnv(ise)
                           (isEnv (hash-set ise (first (rest args)) loc))]
                     [isErr(ise)
                           (isErr ise)])
                   
                   ))])]
           
           [else 
            (cond
              [(empty? vals)
               ;(error 'interp "Arity mismatch: insufficient values")]
               (isErr
                (VError
                 (make-bind-VObj 'TypeError not_in_store 
                                 (VStr (string->list "Arity mismatch: insufficient values")))))]
              
              [else 
               (local
                 ([define loc (next-loc)])
                 (begin
                   (if
                    (isSimpleType (VObj-type (first vals)))
                    (hash-set! store loc (first vals)) 
                    (set! loc (VObj-id (first vals))))
                   #|
         (hash-set (bind-args (rest args) (rest vals) env)
              (first args) loc)
          |#
                   (type-case EnvOrError (bind-args (rest args) (rest vals) env)
                     [isEnv(ise)
                           (isEnv (hash-set ise (first args) loc))]
                     [isErr(ise)
                           (isErr ise)])
                   
                   ))])]))])))




;; constructs a tuple
;;
(define (make-tuple [vars : (listof VObjType)]) : VObjType
  (make-bind-VObj 'list (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                  (VList (make-ListVal vars) #f)))


;; constructs a list
;;
(define (make-ListVal [vars : (listof VObjType)]) : ListVal
  (cond
    [(empty? vars) (mt)]
    [else
     (node (first vars) (make-ListVal (rest vars)))]))



;; mutable global stack of environments
(define menv (box (list (hash (list))))) 


;; mutable global store
(define store (make-hash (list)))




;; Push a new environment on to the environment stack
;;
(define (push-frame (new-env : Env)) : void
  (set-box! menv (cons new-env (unbox menv))))


;; pop an environment off of the stack
;;
(define (pop-frame) : void
  (set-box! menv (rest (unbox menv))))


;; update the current environment
;;
(define (update-frame (new-env : Env)) : void
  (set-box! menv (cons new-env (rest (unbox menv)))))

;(;define (print-store-element [l : Location] [v : VObjType]) : void
;(display "hello"))



;; print the store
;;
(define (print-store) : void
  ;(hash-for-each store print-store-element))
  (display store))



;; print a list of environments
;;
(define (print-envs [envs : (listof Env)]) : (listof void)
  (map print-env envs))



;; print an environment
;;
(define (print-env [env : Env]) : void
  (begin (display "\n")
         (display env)))



;; interp prim1 helper
;;
(define (interp-prim1-aux [op : symbol] [arg : VObjType]) : VAnswer
  (case op
    [(print) (begin (print arg)
                    (display "\n")
                    (VResult arg))]
    [(tagof) (VResult (make-bind-VObj 'string 
                                      (VObj-id 
                                       (VResult-v (lookup 'str (unbox menv)))) 
                                      (VStr (string->list (symbol->string (VObj-type arg))))))]
    [(cast-int)
     (cond 
       [(symbol=? (VObj-type arg) 'int) (VResult arg)]
       [(symbol=? (VObj-type arg) 'bool) 
        (cond
          [(VBool-b (VObj-value arg)) 
           (VResult (make-bind-VObj 'int (VObj-id (VResult-v (lookup 'int (unbox menv)))) (VNum 1)))]
          [else (VResult (make-bind-VObj 'int (VObj-id (VResult-v (lookup 'int (unbox menv)))) (VNum 0)))])]
       [else (error 'casting-int "not valid")])]
    
    [(cast-bool)
     (begin 
       ;(display "h")
       ;(print arg)
       
       (cond 
         [(symbol=? (VObj-type arg) 'bool) 
          (begin 
            (VResult arg)
            )]
         [(symbol=? (VObj-type arg) 'int) 
          (cond
            [(equal? (VNum-n (VObj-value arg)) 0) 
             (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #f)))]
            [else (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #t)))])]
         
         [(symbol=? (VObj-type arg) 'str) 
          (if (empty? (VStr-s (VObj-value arg)))
              (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #f)))
              
              (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #t)))
              
              )]
         [(symbol=? (VObj-type arg) 'dict) 
          
          (if (empty? (hash-keys (VDict-d (VObj-value arg))))
              (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #f)))
              
              (VResult (make-bind-VObj 'bool (VObj-id (VResult-v (lookup 'bool (unbox menv)))) (VBool #t)))
              
              )
          ]
         
         
         [else (error 'casting-bool "not valid")])
       )
     ]
    [(cast-str)
     (begin 
       ;(display "h")
       ;(print arg)
       
       (cond 
         [(symbol=? (VObj-type arg) 'str) 
          (begin 
            (VResult arg)
            )]
         [(symbol=? (VObj-type arg) 'int) 
          (VResult (make-bind-VObj 'str 
                                   (VObj-id (VResult-v (lookup 'str (unbox menv)))) 
                                   (VStr 
                                    (string->list (number->string (VNum-n (VObj-value arg)))))))]
         
         [(symbol=? (VObj-type arg) 'bool) 
          (VResult (make-bind-VObj 'str 
                                   (VObj-id (VResult-v (lookup 'str (unbox menv)))) 
                                   
                                   (VStr
                                    (string->list (if(VBool-b (VObj-value arg))
                                                     "True"
                                                     "False"
                                                     )))))]
         [(symbol=? (VObj-type arg) 'RuntimeError) 
          (VResult (make-bind-VObj 'str 
                                   not_in_store
                                   (begin
                                     ;(display arg)
                                     
                                     ;(display "\n")
                                     (VObj-value arg))))]
         
         
         
         [else (error 'casting-str "not valid")])
       )
     ]
    [(minimum)
     (find-min arg)
     ]
    [(maximum)
     (find-max arg)
     ]
    [(cast-list)
     (cond
       [(mt? (VList-l (VObj-value arg)))
        (VResult (make-bind-VObj 'list
                                 (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                 (VList (mt) #t)))] 
       
       [(not (mt? (node-next (VList-l (VObj-value arg)))))
        (VError
         (make-bind-VObj 'TypeError not_in_store (VStr (string->list "more values than list constr needed"))))]
       
       [else  
        (type-case CVal (VObj-value (node-value (VList-l (VObj-value arg)))) 
          [VList (l m)
                 (VResult (make-bind-VObj 'list
                                          (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                          (VList l #t)))]
          
          [VStr (s) 
                (VResult (make-bind-VObj 'list
                                         (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                         (VList 
                                          (make-ListVal (splitString (list->string s))) #t)))]
          
          [VRange (a) 
                  
                  (range-to-list a)]
          
          
          
          [else
           (mkerr "huh??")])]
       )]
    
    [(cast-tuple)
     (cond
       [(mt? (VList-l (VObj-value arg)))
        (VResult (make-bind-VObj 'tuple
                                 (VObj-id (VResult-v (lookup 'tuple (unbox menv)))) 
                                 (VList (mt) #f)))] 
       
       [(not (mt? (node-next (VList-l (VObj-value arg)))))
        (VError
         (make-bind-VObj 'TypeError not_in_store (VStr (string->list "more values than tuple constr needed"))))]
       
       [else  
        (type-case CVal (VObj-value (node-value (VList-l (VObj-value arg)))) 
          [VList (l m)
                 (VResult (make-bind-VObj 'tuple
                                          (VObj-id (VResult-v (lookup 'tuple (unbox menv)))) 
                                          (VList l #f)))]
          
          [VStr (s) 
                (VResult (make-bind-VObj 'tuple
                                         (VObj-id (VResult-v (lookup 'tuple (unbox menv)))) 
                                         (VList 
                                          (make-ListVal (splitString (list->string s))) #f)))]
          
          [else
           (mkerr "huh?2?")])]
       )]
    
    [(cast-set)
     (cond
       [(mt? (VList-l (VObj-value arg)))
        (VResult (make-bind-VObj 'set
                                 (VObj-id (VResult-v (lookup 'set (unbox menv)))) 
                                 (VSet (hash empty))))] 
       
       [(not (mt? (node-next (VList-l (VObj-value arg)))))
        (VError
         (make-bind-VObj 'TypeError not_in_store (VStr (string->list "more values than set constr needed"))))]
       
       [else  
        (type-case CVal (VObj-value (node-value (VList-l (VObj-value arg)))) 
          [VSet (s)
                (VResult (make-bind-VObj 'set
                                         (VObj-id (VResult-v (lookup 'set (unbox menv)))) 
                                         (VSet s)))]
          
          [VList (l m)
                 (mkerr "adasda")]
          
          [VStr (s) 
                (mkerr "adasda")]
          
          [else
           (mkerr "huh?2?")])]
       )]
    
    [(cast-range)
     (begin
       ;(pln "range get\n\n" arg)
       (VResult 
        (make-bind-VObj 
         'range
         (VObj-id 
          (VResult-v 
           (lookup 'range (unbox menv)))) 
         (VRange (VObj-value arg)))))]
    
    [else
     (error 'prim1 "huh?")]))



;; get the length of a recursive list
;;
(define (length-vlist? [ l : ListVal]) : number
  (cond
    {(mt?  l) 0}
    [else
     (+ 1 (length-vlist? (node-next  l)))]))

;; a will be a tupled args list
 ;;
(define (range-to-list [a : CVal]) : VAnswer
  (begin
    ;(pln " arg" a)
     (local ([define arg-c (length-vlist? (VList-l a))]
            [define str empty])
      (cond 
        [(= 1 arg-c)
         (begin
           (set! str (make-numberlist-two 
                      (make-bind-VObj 'int (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                      (VNum 0))   
                    (node-value (VList-l a))))
           (if
            (and 
             (> (length str) 1)
             (> (first str) (second str)))
            (set! str empty)
            (set! str str))
           ;(pln "str to use " str)
           
           (VResult (make-bind-VObj 'list (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                    (VList (make-ListVal (map packNumObj str)) #t )))
           )]
        [(= 2 arg-c)
         (begin
           (set! str (make-numberlist-two (node-value (VList-l a)) (node-value (node-next (VList-l a)))))
           (if
            (and 
             (> (length str) 1)
             (> (first str) (second str)))
            (set! str empty)
            (set! str str))
           ;(pln "str to use " str)
           (VResult (make-bind-VObj 'list (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                    (VList (make-ListVal (map packNumObj str)) #t ))))]
        
        [(= 3 arg-c)
         (cond
           [ (= 0 (VNum-n (VObj-value (node-value (node-next (node-next (VList-l a)))))))
             
             (VError (make-bind-VObj 
                      'ValueError not_in_store 
                      (VStr (string->list "0 as step"))))]
           [else
            (begin
              (set! str (make-numberlist-two (node-value (VList-l a)) (node-value (node-next (VList-l a)))))
              ;(pln "str to use " str)
              (set! str (slice-numberlist-api
                         str
                         ;(VResult (VObj ;(packNumObj (- (VNum-n (VObj-value (node-value (VList-l a)))) 1)))
                         (VResult (make-bind-VObj 'NoneType not_in_store (VNone)))
                         ;(VResult (packNumObj (- (VNum-n (VObj-value (node-value (node-next (VList-l a))))) 1)))
                         (VResult (make-bind-VObj 'NoneType not_in_store (VNone)))
                         (VResult (packNumObj (abs (VNum-n (VObj-value 
                                                            (node-value (node-next (node-next (VList-l a))))))))) 
                         ))
              (VResult (make-bind-VObj 'list (VObj-id (VResult-v (lookup 'list (unbox menv)))) 
                                       (VList (make-ListVal (map packNumObj str)) #t )))
              )])] 
        [else
         (VError
          (make-bind-VObj 'TypeError not_in_store (VStr (string->list "range arity thing"))))]))))



;; short cut to pack a number object
;;
(define (packNumObj [n : number]) : VObjType
  (make-bind-VObj 'int (VObj-id (VResult-v (lookup 'int (unbox menv)))) (VNum n)))

;; aboslute values of numbers
;;
(define (abs [n : number]) : number
  (if
   (> n 0)
   n
   (* -1 n)))

#|

;;
;;
(define (make-numberlist-one [a : VObjType]) : (listof number)
 (cond
  [(VNum? (VObj-value a)) 
  (count-to (- (VNum-n (VObj-value a)) 1))]
  [else
  (err)]
  ))
|#

;; make number list from f to t
;;
(define (make-numberlist-two [f : VObjType] [t : VObjType]) : (listof number)
  (cond
    [(VNum? (VObj-value f)) 
     (count-to (VNum-n (VObj-value f)) (VNum-n (VObj-value t)))]
    [else
     (err)]))


;; count numbers to list
;;
(define (count-to [f : number] [t : number]) : (listof number)
  (cond
    [(> f t) (reverse (count-to-aux (+ t 1) (+ f 1)))]
    [else (count-to-aux f t)]))



;; counter helper
;;
(define (count-to-aux [f : number] [t : number]) : (listof number)
  (cond
    [(= f t) empty]
    [else
     (append (list f) (count-to-aux (+ f 1)  t))]))




;; slice numbers
;; change? from slice strings
(define (slice-numberlist-api [str : (listof number)] [start-a : VAnswer] 
                              [end-a : VAnswer] [step-a : VAnswer]) : (listof number)
  ;to check Verrors
  (local ([define start (VObj-value (VResult-v start-a))]
          [define end (VObj-value (VResult-v end-a))]
          [define step (VObj-value (VResult-v step-a))])
    
    (begin
      ;(pln "str" str)
      ;(pln "start" start)
      ;(pln "end" end)
      ;(pln "step" step)
      ;check  step exists
      (if
       (VNone? step)
       (set! step (VNum 1))
       (set! step step))
      
      ;check start
      (if
       (VNone? start)
       (if (< (VNum-n step) 0)
           
           ;(set! start (VNum (length str)))
           (set! start (VNum 0))
           
           (set! start (VNum 0))
           )   
       
       (if (< (VNum-n step) 0)
           (set! start (VNum (- (- (length str) (VNum-n start)) 1)))
           (set! start start )))
      
      ;check end
      (if
       (VNone? end)
       (if (< (VNum-n step) 0)
           ;(set! end (VNum 0))
           (set! end (VNum (length str)))
           (set! end (VNum (length str)))
           )   
       
       (if (< (VNum-n step) 0)
           (set! end (VNum (- (length str) (VNum-n end))))
           (set! end end )))
      
      
      
      (slice-numberlist
       (if 
        (< (VNum-n step) 0)
        (reverse str)
        str
        )
       (VNum-n start)
       (VNum-n end)
       (if 
        (< (VNum-n step) 0)
        (* -1 (VNum-n step))
        (VNum-n step))
       
       
       
       ))))


;; slice a list of number
;;
(define (slice-numberlist [str : (listof number)] [start : number] [end : number] [step : number]) : (listof number)
  (begin
    ;(pln "str" str)
    ;(pln "start" start)
    ;(pln "end" end)
    ;(pln "step" step)
    
    (cond
      [(< start 0)
       (slice-numberlist str 0 end step)]
      ;[(< step 0)
      ;(slice-string (reverse str) (- (length str) start) (- (length str) end) (* -1 step))]
      [else
       
       (select-numberlist-from-zero (trim-numberlist-to-slice-start str start) end step 0)]))) 




;; trim a list of number to start from particular num or empty list
;;
(define (trim-numberlist-to-slice-start [str : (listof number)] [start : number]) : (listof number)
  (cond
    [(equal? start 0) 
     str]
    [(> start 0)
     (trim-numberlist-to-slice-start (rest str) (- start 1))]
    [else
     (mkerr "wrong")]))



;; slice number list from first one
;;
(define (select-numberlist-from-zero 
         [str : (listof number)] 
         [end : number] 
         [step : number] 
         [current : number]) : (listof number)  
  (cond
    [(empty? str) empty]
    [(equal? end 0) empty ]
    [(equal? current 0)
     (cons (first str) 
           (select-numberlist-from-zero (rest str)
                                        (- end 1)
                                        step
                                        (if
                                         (< current (- step 1))
                                         (+ current 1)
                                         0)))]
    [else
     (select-numberlist-from-zero (rest str)
                                  (- end 1)
                                  step
                                  (if
                                   (< current (- step 1))
                                   (+ current 1)
                                   0)
                                  )]))

 
;; split string to unit strings
;;
(define (splitString [s : string]) : (listof VObjType)
  (cond
    [(equal? s "") empty]
    [else
     (cons (make-bind-VObj 'str
                           (VObj-id (VResult-v (lookup 'str (unbox menv))))
                           (VStr (cons (first (string->list s)) empty)))
           (splitString (list->string (rest (string->list s)))))]))




;; find minimum
;;
(define (find-min [o : VObjType]) : VAnswer
  (cond
    [(VStr? (VObj-value o))
     (VResult (make-bind-VObj 'str 
                              (VObj-id (VResult-v (lookup 'str (unbox menv)))) 
                              (VStr (string->list (find-min-string (list->string (VStr-s (VObj-value o))) "z")))))]
    [else
     (mkerr "asdas")]))


;; find maximum
;;
(define (find-max [o : VObjType]) : VAnswer
  (cond
    [(VStr? (VObj-value o))
     (VResult (make-bind-VObj 'str 
                              (VObj-id (VResult-v (lookup 'str (unbox menv)))) 
                              (VStr (string->list (find-max-string (list->string (VStr-s (VObj-value o))) "a")))))]
    [else
     (mkerr "asdas")])) 


;; find a minimum char from string
;;
(define (find-min-string [s : string] [start : string]) : string
  (cond
    [(equal? s "") start]
    [else
     (if
      (string<=? (list->string (list (first (string->list s)))) start)
      (find-min-string (list->string (rest (string->list s))) (list->string (list (first (string->list s)))))
      
      (find-min-string (list->string (rest (string->list s))) start))]))



;; find a maximum char from string
;;
(define (find-max-string [s : string] [start : string]) : string
  (cond
    [(equal? s "") start]
    [else
     (if 
      (string>=? (list->string (list (first (string->list s)))) start)
      
      (find-max-string (list->string (rest (string->list s))) (list->string (list (first (string->list s)))))
      (find-max-string (list->string (rest (string->list s))) start))]))





;; make VObj and bind put into env ans store
;;
(define (make-bind-VObj [class_tag : symbol]
                        [class_loc : Location]
                        [thing : CVal]) : VObjType
  (if (isSimpleType class_tag)
      (VObj not_in_store class_tag class_loc thing)
      (local ([define loc (next-loc)]
              [define obj (VObj loc class_tag class_loc thing)])
        (begin
          (hash-set! store loc obj)
          obj))))



;; make a error
;;
(define (mkerr [a : string]) 
  (begin (display "\n")
         (error 'check_a_error a))) 

;; this makes a string from the CVal
;;
(define (make-string-based-on-value [v : CVal]) : string
  (type-case CVal v
    [VNum (n) (constr (list
                       (number->string (length (string->list (number->string n)))) 
                       (number->string n) 
                       (number->string (length (string->list (number->string n))))))]
    [VStr (s) (list->string s)]
    [VBool (b) (if b "True" "False")]
    ;[VClosure (env : Env) (args : (listof symbol)) (body : CExp) (members : Env)]
    [VNone() "None"]
    [VDict (d) (make-string-based-on-Dict d)]
    ;[VMembers (class_tag : symbol) (m : Env)]
    [VList (v m)
           (make-string-based-on-ListVal v )]
    [else
     (begin (display v)
            (error 'makestring "\nnot done"))]))




;; make a compacted string list from a listVal
;;
(define (make-string-based-on-ListVal  [v : ListVal])  : string
  (cond
    [(mt? v) ""]
    [else
     (string-append (make-string-based-on-value (VObj-value (node-value v)))
                    
                    (make-string-based-on-ListVal (node-next v)))]))





;; make a compacted string list from a Dict
;;
(define (make-string-based-on-Dict  [d : (hashof VObjType VObjType)])  : string
  (local ([define keys (hash-keys d)])
    (cond
      [(empty? keys) ""]
      [else
       (constr
        (list (make-string-based-on-value 
               (VObj-value (first keys)))
              
              ;(make-string-based-on-value 
              ;(VObj-value (some-v (hash-ref d (first keys)))))
              (make-string-based-on-Dict
               (hash-remove d (first keys)))))])))



;; remove everything before the char
;;
(define (trimString [c : char] [b : (listof char)]) : (listof char) 
  (cond
    [(empty? b) (list)]
    [(equal? (first b) c) b]
    [else
     (trimString c (rest b))]))



;; get the first n digits
;;
(define (get-firsts [n : number] [b : (listof char)]) : (listof char) 
  (cond
    [(equal? n 0) (list)]
    [else
     (cons 
      (first b)
      (get-firsts (- n 1) (rest b)))]))




;; is a prefix of b?
;;
(define (isPrefix [a : (listof char)] [b : (listof char)]) : boolean
  (cond
    [(< (length b) (length a)) #f]
    
    [(equal? (list->string a)
             (list->string (get-firsts (length a) b)))
     #t]
    [else
     #f]))



;; is a substring of b?
;;
(define (isSubString [a : (listof char)] [b : (listof char)]) : boolean
  (local ([define trimed-b (trimString (first a) b)])
    (cond
      [(> (length a) (length trimed-b)) #f]
      [(isPrefix a trimed-b) #t]
      [else
       (isSubString a (rest b))])))



;; the hash table for inheritence dependency
;;
(define inheritence-hash (make-hash empty))


;; get the updated inheritence table
;;
(define (get-inheritence) 
  (begin 
    (hash-set!
     inheritence-hash (look-and-up 'bool ) (look-and-up 'int ))
    inheritence-hash));



;; if class at loc x is class at loc y 's children
;;
(define (isChild [x : number] [y : number]) : boolean 
  (begin
    ;(pln "x" x)
    
    ;(pln "\ny" x)
    ;(pln "\nd" get-inheritence)
    ;(display "\n")
    (type-case (optionof number) (hash-ref (get-inheritence) x)
      [some(v) (equal? v y)]
      [none() #f])))





;; interp a list of CExp (body of class)
;;
(define (interp-body [l : (listof CExp)] [oldEnv : Env] [newEnv : Env]) : EnvOrError
  (if (empty? l)
      ;(isEnv newEnv)
      (isEnv (hash-union oldEnv newEnv))
      (begin
        (push-frame oldEnv)
        (local
          ([define v-answer (interp (first l))]
           [define tempEnv (first (unbox menv))]
           [define newEnv2 (hash-union newEnv (hash-diff tempEnv oldEnv))])
          (begin
            (pop-frame)
            ;(pln "oldEnv" oldEnv)
            ;(pln "\n" "\n")
            ;(pln "tempEnv" tempEnv)
            ;(pln "\n" "\n")
            ;(pln "newEnv" newEnv)
            ;(pln "\n" "\n")
            ;(pln "newEnv2" newEnv2)
            ;(pln "\n" "\n")
            ;(pln "(hash-diff tempEnv oldEnv)" (hash-diff tempEnv oldEnv))
            ;(pln "\n" "\n")      
            ;(pln "\n" "\n")
            ;(local
            ;([define recEnv (interp-body (rest l) oldEnv tempEnv)])
            ;(if (isErr? recEnv)
            ;recEnv
            ;(isEnv (hash-union (isEnv-e recEnv) newEnv)))))))))
            (if (VError? v-answer)
                (isErr v-answer)
                (interp-body (rest l) oldEnv newEnv2)))))))




;; set union 
;;
(define (hash-union [d : (hashof 'a 'b)] [ to-sub : (hashof 'a 'b)]) : (hashof 'a 'b)
  (hash-uinion-aux d (hash-keys to-sub) to-sub))




;; helper for set union
;;
(define (hash-uinion-aux [d : (hashof 'a 'b)] [keys : (listof 'a )] [to-add : (hashof 'a 'b)]) : (hashof 'a 'b)
  (cond
    [(empty? keys) d]
    [else
     (hash-set (hash-uinion-aux d (rest keys) to-add) (first keys)
               (some-v (hash-ref to-add (first keys)))
               )]))


;; set subtraction 
;;
(define (hash-sub [d : (hashof 'a 'b)] [ to-sub : (hashof 'a 'b)]) : (hashof 'a 'b)
  (hash-sub-aux d (hash-keys to-sub)))


;; set subtraction helper
;;
(define (hash-sub-aux [d : (hashof 'a 'b)] [keys : (listof 'a )]) : (hashof 'a 'b)
  (cond
    [(empty? keys) d]
    [else
     (hash-remove (hash-sub-aux d (rest keys)) (first keys))]))



;; set intersection
;;
(define (hash-inter [d1 : (hashof 'a 'b)] [d2 : (hashof 'a 'b)]) : (hashof 'a 'b)
  (hash-sub d1 (hash-sub d1 d2))) 



;; set difference
;;
(define (hash-diff [d1 : (hashof 'a 'b)] [d2 : (hashof 'a 'b)]) : (hashof 'a 'b)
  (local 
    ([define keys (hash-keys d1)])
    (cond
      [(empty? keys) d1]
      [else
       (local
         ([define key (first keys)])
         (if (equal? (hash-ref d1 key) (hash-ref d2 key))
             (begin
               ;(pln "\nEquals" key)
               ;(pln "D1" (hash-ref d1 key))
               ;(pln "D2" (hash-ref d2 key))
               (hash-diff (hash-remove d1 key) (hash-remove d2 key)))
             (begin
               
               ;(pln "\nNot Equals" key)
               (hash-set 
                (hash-diff (hash-remove d1 key) (hash-remove d2 key))
                key
                (some-v (hash-ref d1 key))))))])))
