#lang plai-typed

;; Surface syntax for Python
;;
(define-type PyExpr
  [PySeq     (es : (listof PyExpr))]
  [PyNum     (n : number)]
  [PyId      (x : symbol)]
  [PyApp     (fun : PyExpr) (args : (listof PyExpr))]
  [PyStr     (s : (listof string))]
  [PyIf      (t : PyExpr) (b : PyExpr) (e : PyExpr)]
  [PyRaise   (e : PyExpr)] 
  [PyPass]
  [PyBoolOp  (s : symbol) (ls : (listof PyExpr))]
  [PyUnaryOp (s : symbol) (operand : PyExpr)]
  [PyBinOp   (s : symbol) (l : PyExpr) (r : PyExpr)]
  [PyCompare (s : PyExpr) (ops : (listof symbol)) (comparators  : (listof PyExpr))]
  [PyDict    (keys : (listof PyExpr)) (values : (listof PyExpr))]
  [PySet    (values : (listof PyExpr))]
  [PyLambda  (ps : (listof symbol)) (b : PyExpr)]
  [PyFunc    (n : symbol) (ps : (listof symbol)) (b : (listof PyExpr))]
  [PySet!    (lhs : (listof PyExpr)) (v : PyExpr)] 
  [PyReturn  (v :  PyExpr)]
  [PyClass   (n : symbol) (b : (listof PyExpr))]
  [PyDot     (o : PyExpr) (f : symbol)]
  [PyTryFinally (try :  PyExpr) (final : PyExpr)]
  [PyTryExcept (try : PyExpr) (excepthandlers : (listof PyExpr)) (else : PyExpr)]
  [PyExpHandler (name : symbol) (class_tag : string) (body : PyExpr)]
  [PyList(l : (listof PyExpr))]
  [PyTuple(l : (listof PyExpr))]
  [PySubscript (l : PyExpr) (index : PyExpr)]
  [PySlice (s : PyExpr) (e : PyExpr) (step : PyExpr)]
  [PyDelete (target : PyExpr)]
  [PyAugAssign(t : PyExpr)  (op : symbol)(v : PyExpr)]
  [PyGlobal(x : (listof symbol))]
  [PyNonlocal(x : (listof symbol))]
  )

