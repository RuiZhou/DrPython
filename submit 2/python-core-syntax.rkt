#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum     (n : number)]
  [CStr     (s : (listof string))]
  [CBool    (b : boolean)]
  [CSeq     (e1 : CExp) (e2 : CExp)]
  [CError   (e1 : CExp)]
  [CIf      (test : CExp) (then : CExp) (else : CExp)]
  [CId      (x : symbol)]
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
  [CSet!(lhs : symbol) (v : CExp)]  
  [CReturn (v : CExp)]
  [CClass (n : symbol) (b : CExp) (vs : (listof symbol))]
  ; A COBJ that will be interped into vmembers
  [CInstance(class_tag : symbol) (loc  : Location)]
  [CGetField (o : CExp) (s : symbol)]
  
  )
;(define-type CObjType
; [CObj (id : string) (type : symbol) (value : CExp)])




(define-type CVal
  [VNum (n : number)]
  [VStr (s : (listof string))]
  [VBool (b : boolean)]
  [VClosure (env : Env) (args : (listof symbol)) (body : CExp) (members : Env)]
  [VNone]
  [VDict (d : (hashof VObjType VObjType))]
  [VMembers (class_tag : symbol) (m : Env)]
  )


(define-type VObjType
  [VObj  (id : string) 
         (type : symbol)  ; Replace with class name?
         (class_loc : Location)
         (value : CVal)]
  )


(define-type VAnswer
  [VResult (v : VObjType) ]
  [VReturn (v : VObjType)]
  [VError (v : VObjType)])

;have type systems to enforce more safety 
; eliminate potential errors
; return/excpetion/control flow  /return /raise
; class storage/try to push things classy? through
; overloading operators
; how to handle ovloading
; __add__..give a thought
#|

(define-type CObj
  [CAnonObj (id : string) (type : symbol) (value : CVal)])
|#
(define-type-alias Location number)

(define-type-alias Env (hashof symbol Location))

(define-type-alias Stack (boxof (listof Env)))

(define-type-alias Store (boxof (hashof Location VObjType)))



