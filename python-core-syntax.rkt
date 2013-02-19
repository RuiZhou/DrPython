#lang plai-typed

;; Retrieve a new location for the store
;;
(define next-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))


;things not in store
(define not_in_store 
  ;  (lambda() (* -1 (next-loc)) ))
  -1)

;; Core Language of Python
;;
(define-type CExp
  [CNum     (n : number)]
  [CStr     (s : (listof char))]
  [CBool    (b : boolean)]
  [CSeq     (e1 : CExp) (e2 : CExp)]
  [CError   (class_tag : symbol) (e : CExp)]
  [CIf      (test : CExp) (then : CExp) (else : CExp)]
  [CId      (x : symbol)]
  ;[CRef     (x : symbol)]
  [CLet     (x : symbol) (bind : CExp) (body : CExp)]
  [CApp     (fun : CExp) (args : (listof CExp))]
  [CLambda  (args : (listof symbol)) (body : CExp)]
  [CPrim1   (prim : symbol) (arg : CExp)]
  [CRaise   (e : CExp)]
  [CPass]
  [CBoolOp  (op : symbol) (l : CExp) (r : CExp)]
  [CUnaryOp (op : symbol) (operand : CExp)]
  [CBinOp   (op : symbol) (l : CExp) (r : CExp)]
  [CEmpty?  (l : CExp)]
  [CFirst   (l : CExp)]
  [CRest    (l : CExp)]
  [CCompare (op : symbol) (l : CExp) (r : CExp)]
  [CNone]
  [CDict(keys : (listof CExp)) (values : (listof CExp))]
  [CSet (values : (listof CExp))]
  [CDictGet (d : CExp) (k : CExp) (df : CExp )]
  [CDictUpdate  (d : CExp)   (df : CExp )]
  [CDictKeys (d : CExp)]
  [CDictValues (d : CExp)]
  [CDictItems (d : CExp)]
  [CSet!(lhs : CExp) (v : CExp)]
  [CCopy!(lhs : symbol) (v : CExp)] 
  [CReturn (v : CExp)]
  [CClass (n : symbol) (b : (listof CExp)) (vs : (listof symbol))]
  ; A COBJ that will be interped into vmembers
  [CInstance(class_tag : symbol) (loc  : Location)]
  [CGetField (o : CExp) (s : symbol)]
  [CStrId(pointer : CExp) ]
  ;try (excepthandler else); we will add else anyway
  [CTryEx (t : CExp) (eh : CExp) (orelse : CExp)]
  ;CFinally because we should not add finally if there is not finally
  [CFinally (te : CExp) (f : CExp)]
  [CList (m : boolean)]
  [CCons (v : CExp) (r : CExp)] 
  [CUndefined]
  [CIsInstance (i : CExp) (c : CExp)]
  
  ; this may be reduced to a function only
  [CSubscript(object : CExp) (index : CExp) ]
  [CSlice (s : CExp) (e : CExp) (step : CExp)]
  ; get the id
  [CGetObjId (o : CExp)]
  
  ; get the class id
  [CGetClassId (o : CExp)]
  [CDelete (t : CExp)]
  
  [CNonlocal (x : symbol)]
  )

;(define-type CObjType
; [CObj (id : string) (type : symbol) (value : CExp)])



;; Value representation of data
;;
(define-type CVal
  [VNum (n : number)]
  [VStr (s : (listof char))]
  [VBool (b : boolean)]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp) (members : Env)]
  [VNone]
  [VDict (d : (hashof VObjType VObjType))]
  [VSet (s : (hashof CVal VObjType))]
  [VMembers (class_tag : symbol) (m : Env)]
  [VList (l : ListVal) (m : boolean)]
  ;[VRef (loc : Location)]
  [VRange (args : CVal)]
  )

;; Recursive list structure
;;
(define-type ListVal
  [mt]
  [node (value : VObjType) (next : ListVal)])



;; Object structure that holds all answers in Python
;;
(define-type VObjType
  [VObj  (id : Location) 
         (type : symbol)  ; Replace with class name?
         (class_loc : Location)
         (value : CVal)]
  )

;; Removes the location information from an object
;;
(define (remove-loc-info [in : VObjType]) : VObjType
  (VObj  0 
         (VObj-type in)   
         (VObj-class_loc in)
         (local ([define cv  (VObj-value in) ])
           (type-case CVal cv
             [VNum (n)  cv]
             [VStr (s ) cv]
             [VBool (b )  cv]
             [VClosure (env  args     body  members  ) cv]
             [VNone() cv]
             [VDict (d) (VDict (remove-loc-info-hash d) )]
             [VSet (s) (VSet (remove-loc-info-hash2 s))]
             [VMembers (class_tag  m ) cv]
             [VList (l m) cv ]
             [VRange (a) cv]
             ))
         ))


;; Removes the location information from hash table
;;
(define (remove-loc-info-hash [h : (hashof VObjType VObjType)] ) :  (hashof VObjType VObjType)
  (local ([define keys (hash-keys h)]) 
    (remove-loc-info-hash-aux h  keys)))


;; Removes the location information from hash table
;;
(define (remove-loc-info-hash-aux [h : (hashof VObjType VObjType)]  [keys : (listof VObjType)]) :  (hashof VObjType VObjType)
  (cond
    [(equal? (length keys) 0) (hash empty)]
    [else 
     (hash-set  (remove-loc-info-hash-aux h  (rest keys)) 
                (remove-loc-info (first keys) )
                (remove-loc-info (some-v  (hash-ref h (first keys) ))))])) 

;; Removes the location information from hash table
;;
(define (remove-loc-info-hash2 [h : (hashof CVal VObjType)] ) :  (hashof CVal VObjType)
  (local ([define keys (hash-keys h)]) 
    (remove-loc-info-hash2-aux h  keys)))


;; Removes the location information from hash table
;;
(define (remove-loc-info-hash2-aux [h : (hashof CVal VObjType)]  [keys : (listof CVal)]) :  (hashof CVal VObjType)
  (cond
    [(equal? (length keys) 0) (hash empty)]
    [else 
     (hash-set  (remove-loc-info-hash2-aux h  (rest keys)) 
                (first keys)
                (remove-loc-info (some-v  (hash-ref h (first keys) ))))])) 


;; Control structure for returned value in the interp function
;;
(define-type VAnswer
  [VResult (v : VObjType) ]
  [VReturn (v : VObjType)]
  [VError (v : VObjType)])


(define-type-alias Location number)

(define-type-alias Env (hashof symbol Location))

(define-type-alias Stack (boxof (listof Env)))

(define-type-alias Store (boxof (hashof Location VObjType)))



