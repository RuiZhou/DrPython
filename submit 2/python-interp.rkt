#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "python-helper.rkt"
         )

(define (wrap-interp [v-answer : VAnswer]) : void
  (type-case VAnswer v-answer
    [VResult (v)  (void) ]
    [VReturn (v)   (error 'interp  "Syntax Error")]
    [VError (v) (error 'interp  "Error")]))


(define (interp [expr : CExp]) : VAnswer
  (local 
    ([define env (first (unbox menv))])
    (type-case CExp expr
      [CNum (n) (VResult (VObj "id" 'int not_in_store (VNum n) ))]
      
      [CStr (s) (VResult (VObj "id" 'str not_in_store (VStr s) ))]    
      
      [CError (e) 
              (local 
                ([define exp (interp e)])
                (type-case VAnswer exp
                  [VResult (v) (VResult (VObj "id" 'Exception  not_in_store (VObj-value (VResult-v exp)) ))]
                  [VReturn (v) (error 'interp  "Syntax Error")]
                  [VError (v)  exp]))] ;TODO
      
      [CIf (t b e) 
           (local ([define test-answer (interp t)])
             (type-case VAnswer test-answer
               [VResult (v) (cond
                              [(isTrue (VResult-v test-answer)) (interp b)]
                              [else (interp e)])]
               [VReturn (v) (error 'interp  "Syntax Error")]
               [VError (v)  test-answer]))]
      
      
      
      [CId (x) (lookup x (unbox menv))]
                    
      
      [CLet (x bind body)
            (local
              ([define exp (interp bind)])
              (type-case VAnswer exp
                [VReturn (v) (error 'interp  "Syntax Error")]
                [VError (v)  exp]
                [VResult (v) 
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
                                 value))))]))]
      
      [CSeq (e1 e2)
            (local ([define e1-value (interp e1)])
              (type-case VAnswer e1-value
                [VReturn (v) (VResult v)]
                [VError (v)  e1-value]
                [VResult (v) (interp e2)]))]
      
      
      
      [CApp (fun arges)
            (local
              ([define funAnswer (interp fun)])
              (type-case VAnswer funAnswer
                [VReturn (v) (error 'interp  "Syntax Error")]
                [VError (v)  funAnswer]
                [VResult (funObj) 
                         ;(cond [(symbol=? (VObj-type funObj) 'func)
                         (type-case CVal (VObj-value funObj)
                           [VClosure (e a b m)
                                     (local 
                                       [(define arg-answers (map interp arges))
                                        (define bad-syntax (filter VReturn? arg-answers))
                                        (define errors (filter VError? arg-answers))]
                                       (cond
                                         [(not (empty? bad-syntax )) (error 'interp  "Syntax Error")]
                                         [(not (empty? errors )) (first errors)]
                                         [else (local 
                                                 [(define argvs (map VResult-v arg-answers))]
                                                 (begin
                                                   (push-frame 
                                                    (bind-args a argvs e))
                                                   (local
                                                     ([define value (interp b)])
                                                     (begin 
                                                       (pop-frame)
                                                       (type-case VAnswer value
                                                         [VReturn (v) (VResult v)]
                                                         [VError (v)  value]
                                                         [VResult (v) value])))))]))]
                           
                           [else (error 'interp "Not a closure")])]))]
      
      
      
      
      [CLambda (args body) (VResult (VObj "id" 'func  not_in_store  (VClosure env args body (hash (list)) ) ))] 
      
      [CPrim1 (prim arg)
              (local 
                ([define arg-answer (interp arg)]) 
                (type-case VAnswer arg-answer
                  [VReturn (v)  (error 'interp-cprim1  "Syntax Error")]
                  [VError (v)  arg-answer]
                  [VResult (v) (python-prim1 prim (VResult-v arg-answer))]))]
      
      
      [CBool (b) (VResult (VObj "id" 'bool not_in_store (VBool b) ))]
      
      [CRaise (e) 
              (local 
                ([define e-answer (interp e)])
                (type-case VAnswer e-answer
                  [VReturn (v)  (error 'interp  "Syntax Error")]
                  [VError (v)  e-answer]
                  [VResult (v) (VError (VResult-v e-answer))]))]
      
      
      
      [CPass() (VResult (VObj "id" 'NoneType not_in_store  (VNone) ))] 
      
      [CBoolOp (op l r) (interp-CBoolOp op l r)] 
      
      [CUnaryOp (op operand)  (interp-CUnaryOp op operand)]
      
      [CBinOp (op l r)  (interp-CBinOp op l r)]
      
      [CEmpty? (l)
               (local 
                 ([define l-answer (interp l)])
                 (type-case VAnswer l-answer
                   [VReturn (v)  (error 'interp  "Syntax Error")]
                   [VError (v)  l-answer]
                   [VResult (v) (VResult (VObj "id" 'bool not_in_store  (VBool (empty? (VStr-s (VObj-value v)))) ))]))]
      
      [CFirst (l)
              (local 
                ([define l-answer (interp l)])
                (type-case VAnswer l-answer
                  [VReturn (v)  (error 'interp  "Syntax Error")]
                  [VError (v)  l-answer]
                  [VResult (v) (VResult (VObj "id" 'str  not_in_store (VStr (list (first (VStr-s (VObj-value v))))) ))]))]
      
      [CRest (l)
             (local 
               ([define l-answer (interp l)])
               (type-case VAnswer l-answer
                 [VReturn (v)  (error 'interp  "Syntax Error")]
                 [VError (v)  l-answer]
                 [VResult (v) (VResult (VObj "id" 'str  not_in_store (VStr  (rest (VStr-s (VObj-value v)))) ))]))]
      
      
      
      [CCompare (op left right)
                (interp-CCompare op left right)]
      
      [CNone() (VResult (VObj "id" 'NoneType  not_in_store  (VNone) ))]
      
      
      [CDict (keys values)
             (interp-CDict keys values   (hash empty))]
      
      
      [CSet! (lhs v)
             (local ([define v-answer (interp v)])
               (type-case VAnswer v-answer 
                 [VReturn (v)  (error 'interp  "Syntax Error")]
                 [VError (v) v-answer]
                 [VResult (value)
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
                                         v-answer))])]))]
      
      
      [CReturn (v)
               (local ([define v-answer  (interp v)])
                 (type-case VAnswer v-answer
                   [VResult (v) (VReturn v)]
                   [VReturn (v)  (error 'interp  "Syntax Error")]
                   [VError (v) v-answer]))]
      
      [CGetField (o s)
                 (local ([define v-answer  (interp o)])
                   (type-case VAnswer v-answer
                     [VResult (v) 
                              (type-case CVal (VObj-value v)
                                [VMembers (ct m)
                                          (local 
                                            ([define class (hash-ref store (VObj-class_loc v))])
                                            (type-case (optionof VObjType) class
                                              [some (v) 
                                                    (lookup s (list m (VClosure-members (VObj-value v))))]
                                              [none() (error 'interp "Class not in store")]))]
                                [VClosure (env args body members)
                                          (lookup s (list members))]
                                [else (error 'interp  "Syntax Error")])]
                     [VReturn (v)  (error 'interp  "Syntax Error")]
                     [VError (v) v-answer]))]
      
      
      ; Interp [CClass (n : symbol) (b : CExp) (vs : (listof symbol))]       
      ; to 
      ; [VClosure (env : Env) (args : (listof symbol)) (body : CExp) (members : Env)]
      [CClass (n b vs)
              (local 
                ;class tag ?? just use the symbol?
                ;([define class_tag (string->symbol (string-append "class: " (symbol->string n)))])
                ([define class_tag n])
                (local ([define loc (next-loc)])
                (VResult
                 (VObj "id"  class_tag; class ... 
                       not_in_store 
                       (local
                         ; the actual colosure which includes the constructor function and the 
                         ; static memebers
                         ([define constructor
                            (VClosure 
                             env ; make a copy of current VAnswerenvironment
                             empty; empty means take no parameter in constructor
                             (CInstance class_tag loc)  ;body to make a new object
                             (begin  ; the static members
                               ;first we push a empty env
                               (push-frame (hash empty))
                               ;now add all the hosted variable as undefined
                               ;(begin (add-undefined-to-store)
                                      (begin (bind-hoisted-variables vs)
                                             (local ([define v-answer  (interp b)])
                                               (type-case VAnswer v-answer
                                                 [VResult (v) (first (unbox menv))]
                                                 [VReturn (v)  (error 'interp  "Syntax Error")]
                                                 [VError (v) (error 'interp  "Syntax Error")])))))])
                         (begin (pop-frame) ; pop the old  frame
                                ;(local ([define loc (next-loc)])
                                  (begin; bind the classinto env 
                                    (update-frame 
                                     (hash-set env class_tag loc))
                                    (begin ; write the class into store
                                      (hash-set! store loc  
                                                 (VObj  "id"   'class  not_in_store  constructor))
                                      constructor))))))))]
      
      
      [CInstance (t l) 
                 ;(local 
                 ; ([define found (lookup t (unbox menv))])
                 ;(type-case  VAnswer found
                 ; [VResult (v) 
                 (VResult 
                  (VObj "id" t l 
                        ; (string->symbol 
                        ; (string-append "instance of" 
                        ;               (symbol->string t)))
                        (VMembers t (hash (list)))))]
      ;[VReturn (v)  (error 'interp  "Syntax Error")]
      ;[VError (v) found]))
      
      
                   
               
               ;    [none () (error 'interp (string-append "Class not exist: " (symbol->string t)))])]
      
      ;[else (begin (printCExp expr) (error 'interp "dummy1" ))] 
      
      )))


#|
; helpe method : bind undefined into the current env and store
; we assume the location -1 is undefined 
(define (add-undefined-to-store ) : void
  (hash-set! store -1 (VObj "id" 'undefined (VNone))))
|#

; help  method to bind all the hosted variables
(define (bind-hoisted-variables [vs : (listof symbol)] ) : void
  (cond 
    [(not (empty? vs)) ;; add case for no hoisted variables
     (begin
       (local ([define loc (next-loc)])
         (begin   (update-frame (hash-set (first (unbox menv)) (first vs)  loc))
                  (hash-set! store loc (VObj "id" 'undefined not_in_store  (VNone)))))
       (bind-hoisted-variables (rest vs)  ))]
    [else (void)]))


(define (lookup [x : symbol] [envs : (listof Env)]) : VAnswer
  (type-case (optionof VObjType) (hash-ref store  (look-and-up-with-env x envs))
    [some (v) 
          (cond
            [(symbol=? (VObj-type v) 'undefined)
             (error 'interp (string-append "Undefined  identifier : " (symbol->string x) ))]
            [else (VResult v)])]
    [none()(error 'interp (string-append "Undefined  identifier : " (symbol->string x) ))]))


;;; look up through levels of environments
;;; with the current environment
;(define (look-and-up x) : Location
;  (look-and-up-with-env x (unbox menv)))

;;; look up through levels of environments
(define (look-and-up-with-env [x : symbol] [envs : (listof Env)] ) : Location
  (cond 
    [(empty? envs) (error 'interp (string-append "Undeclared  identifier : " (symbol->string x) ))]
    [else  (type-case (optionof Location) (hash-ref (first envs) x)
             [some (l) l]
             [none() (look-and-up-with-env x (rest envs))])]))





(define (interp-CDict [keys : (listof CExp)] 
                      [values : (listof CExp)] 
                      [h : (hashof VObjType VObjType)]) :  VAnswer
  (cond
    [(empty?  keys) (VResult  (VObj "id" 'dict  not_in_store (VDict h) ))]
    [else
     (local ([define  k-answer  (interp (first keys))])
       (type-case VAnswer k-answer
         [VReturn (k)  (error 'interp-CDict  "Syntax Error")]
         [VError (k)  k-answer]
         [VResult (k) 
                  (local ([define  v-answer  (interp (first values))])
                    (type-case VAnswer v-answer
                      [VReturn (v)  (error 'interp-CDict  "Syntax Error")]
                      [VError (v)  v-answer]
                      [VResult (v) 
                               (interp-CDict (rest keys) (rest values) (hash-set h k v))]))]))]))



(define (interp-CCompare  [op : symbol] [left : CExp]  [right : CExp]) : VAnswer
  (local ([define l-answer (interp left)]
          [define r-answer (interp right)])
    (type-case VAnswer l-answer
      [VReturn (v) (error 'interp  "Syntax Error")]
      [VError (v)  l-answer]
      [VResult (l-obj) 
               (type-case VAnswer r-answer
                 [VReturn (v) (error 'interp  "Syntax Error")]
                 [VError (v)  r-answer]
                 [VResult (r-obj) 
                          
                          (cond 
                            [(VNum? (VObj-value l-obj))
                             (cond 
                            [(symbol=? 'noteq op)
                             (VResult (VObj "id" 'bool  not_in_store  (VBool (not (= (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj))))) )) ]
                            [(symbol=? 'eq op)
                             (VResult (VObj "id" 'bool not_in_store   (VBool  (= (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj)))) )) ]
                            [(symbol=? 'lt op)
                             (VResult (VObj "id" 'bool  not_in_store  (VBool (< (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj)))) )) ]
                            [(symbol=? 'gt op)
                             (VResult (VObj "id" 'bool   not_in_store (VBool (> (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj)))) )) ]
                            [(symbol=? 'lte op)
                             (VResult (VObj "id" 'bool  not_in_store  (VBool (<= (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj)))) )) ]
                            [(symbol=? 'gte op)
                             (VResult (VObj "id" 'bool  not_in_store  (VBool (>= (VNum-n (VObj-value l-obj)) (VNum-n (VObj-value r-obj)))) )) ]
                            [else (error 'interp  "not taken care of 1")])]
                          
                            [(VBool? (VObj-value l-obj))
                             (cond 
                            [(symbol=? 'noteq op)
                             (VResult (VObj "id" 'bool  not_in_store  (VBool (not (equal? (VBool-b (VObj-value l-obj)) (VBool-b (VObj-value r-obj))))) )) ]
                            [(symbol=? 'eq op)
                             (VResult (VObj "id" 'bool not_in_store   (VBool  (equal? (VBool-b (VObj-value l-obj)) (VBool-b (VObj-value r-obj)))) )) ]
                            [else (error 'interp  "not taken care of 1")])]
                          
                            
                            
                          )
                          ])])))




(define (interp-CUnaryOp [op : symbol] [operand : CExp]) : VAnswer
  (local ([define operand-answer (interp operand)])
    (type-case VAnswer operand-answer
      [VReturn (v) (error 'interp  "Syntax Error")]
      [VError (v)  operand-answer]
      [VResult (operand-obj) 
               (cond 
                 [(symbol=? op 'not)
                  (VResult (VObj "id" 'bool  not_in_store (VBool (not (isTrue operand-obj))) ))]
                 [(symbol=? op 'usub)
                  (VResult (VObj "id" 'int not_in_store  (VNum (* -1 (VNum-n (VObj-value operand-obj)))) ))]
                 [else (error 'interp  "not taken care of 4")]  )])))


(define (interp-CBoolOp (op : symbol) (l : CExp) (r : CExp) ) : VAnswer
  (local ([define l-answer (interp l)]
          [define r-answer (interp r)])
    (type-case VAnswer l-answer
      [VReturn (v) (error 'interp  "Syntax Error")]
      [VError (v)  l-answer]
      [VResult (l-obj) 
               (type-case VAnswer r-answer
                 [VReturn (v) (error 'interp  "Syntax Error")]
                 [VError (v)  r-answer]
                 [VResult (r-obj) 
                          (cond 
                            [(symbol=? op 'or) 
                             (cond
                               [(isTrue l-obj)  l-answer]
                               [else  r-answer])]
                            [(symbol=? op 'and) 
                             (cond
                               [(isTrue (VResult-v l-answer))  r-answer]
                               [else  l-answer])]
                            [else (error 'interp  "not taken care of 2")]
                            )])])))


(define (interp-CBinOp (op : symbol) (l : CExp) (r : CExp)) : VAnswer
  (local ([define l-answer (interp l)]
          [define r-answer (interp r)])
    (type-case VAnswer l-answer
      [VReturn (v) (error 'interp  "Syntax Error")]
      [VError (v)  l-answer]
      [VResult (v) 
               (type-case VAnswer r-answer
                 [VReturn (v) (error 'interp  "Syntax Error")]
                 [VError (v)  r-answer]
                 [VResult (v) 
                          (cond 
                            [(VNum? (VObj-value (VResult-v l-answer)))
                             (cond 
                               [(symbol=? op 'add);TODO type checks and more 
                                (VResult (VObj "id" 'int not_in_store   (VNum (+ (VNum-n (VObj-value (VResult-v l-answer))) (VNum-n (VObj-value (VResult-v r-answer))))) ))]
                               [(symbol=? op 'sub);TODO type checks and more 
                                (VResult (VObj "id" 'int not_in_store   (VNum (- (VNum-n (VObj-value (VResult-v l-answer))) (VNum-n (VObj-value (VResult-v r-answer))))) ))]
                               [(symbol=? op 'mult);TODO type checks and more 
                                (VResult (VObj "id" 'int not_in_store   (VNum (* (VNum-n (VObj-value (VResult-v l-answer))) (VNum-n (VObj-value (VResult-v r-answer))))) ))]
                               [else (error 'interp  "not taken care of 3")])]
                         #|   
                            [([VBool (b : boolean)]? (VObj-value (VResult-v l-answer)))
                             (cond 
                               [(symbol=? op 'add);TODO type checks and more 
                                (VResult (VObj "id" 'int not_in_store   (VNum (+ (VNum-n (VObj-value (VResult-v l-answer))) (VNum-n (VObj-value (VResult-v r-answer))))) ))]
                               [(symbol=? op 'sub);TODO type checks and more 
                                (VResult (VObj "id" 'int not_in_store   (VNum (- (VNum-n (VObj-value (VResult-v l-answer))) (VNum-n (VObj-value (VResult-v r-answer))))) ))]
                               [(symbol=? op 'mult);TODO type checks and more 
                                (VResult (VObj "id" 'int not_in_store   (VNum (* (VNum-n (VObj-value (VResult-v l-answer))) (VNum-n (VObj-value (VResult-v r-answer))))) ))]
                               [else (error 'interp  "not taken care of 3")])]
                             
                           |#  
                             
                          )
                          ])])))




(define (isTrue (object : VObjType)) : boolean
  (cond
    [(symbol=? 'str (VObj-type object))
     (cond 
       [(empty? (VStr-s (VObj-value object))) #f]
       [else  #t])]
    [(symbol=? 'bool (VObj-type object))
     (VBool-b (VObj-value object))]
    [(symbol=? 'int (VObj-type object))
     (cond 
       [(= 0 (VNum-n (VObj-value object))) #f]
       [else #t])]
    [(symbol=? 'NoneType (VObj-type object))  #f]
    [else #t]))




(define (bind-args [args : (listof symbol)] [vals : (listof VObjType)] [env : Env])
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (local
           ([define loc (next-loc)])
           (begin
             (hash-set! store loc (first vals)) 
             (hash-set (bind-args (rest args) (rest vals) env)
                       (first args) loc)))]
        [else (error 'interp  "not taken care of 5")]
        ))

;things not in store
(define not_in_store -1)

;mutable stack env
(define menv (box (list (hash (list))))) 

(define store (make-hash (list)))

(define next-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define (push-frame (new-env : Env) ) : void
  (set-box! menv (cons new-env (unbox menv))))

(define (pop-frame) : void
  (set-box! menv (rest (unbox menv))))

(define (update-frame (new-env : Env)) : void
  (set-box! menv (cons new-env (rest (unbox menv)))))


