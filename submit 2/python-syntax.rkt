#lang plai-typed

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
  [PyLambda  (ps : (listof symbol)) (b : PyExpr)]
  [PyFunc    (n : symbol) (ps : (listof symbol)) (b : (listof PyExpr))]
  [PySet!    (lhs : (listof symbol)) (v : PyExpr)] 
  [PyReturn  (v :  PyExpr)]
  [PyClass   (n : symbol) (b : (listof PyExpr))]
  [PyDot     (o : PyExpr) (f : symbol)]
  ;[PyBracket ()]
  ;[PyDotMethod()]
  ;[PyBracketMethod()]
  )

