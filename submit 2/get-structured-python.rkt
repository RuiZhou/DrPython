#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

;(require racket/base)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define (get-structured-python pyjson)
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PySeq (map get-structured-python expr-list))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]
    
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list))]
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr (map string (string->list s)))]
    
    [(hash-table ('nodetype "If")
                 ('test test) 
                 ('body body) 
                 ('orelse orelse))
     (PyIf (get-structured-python test)
           (PySeq (map get-structured-python body))
           (PySeq (map get-structured-python orelse)) )]
    ;#hasheq((body . (#hasheq((type . "Raise") (cause . #\nul) (exc . #\nul))))
    ;        (type . "Module"))
    
    
    [(hash-table ('nodetype "Raise")
                 ('cause cause)
                 ('exc exc))
     (PyRaise (get-structured-python  exc))
     ;   (error 'passe exc)
     ]
    
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    
    [(hash-table ('nodetype "BoolOp")
                 ('op op)
                 ('values values))
     (PyBoolOp (get-structured-python op)
               (map get-structured-python values))]
    
    [(hash-table ('nodetype "Or"))
     'or]
    
    [(hash-table ('nodetype "And"))
     'and]
    
    [(hash-table ('nodetype "Not"))
     'not]
    
    [(hash-table('nodetype "UnaryOp")
                ('op op)
                ('operand operand))
     (PyUnaryOp (get-structured-python op) 
                (get-structured-python operand))]
    
    
    [(hash-table('nodetype "BinOp")
                ('op op)
                ('left left)
                ('right right))
     (PyBinOp (get-structured-python op)
              (get-structured-python left)
              (get-structured-python right))]
    
    
    [(hash-table ('nodetype "Add"))
     'add]
    
    [(hash-table ('nodetype "Mult"))
     'mult]
    
    [(hash-table ('nodetype "Sub"))
     'sub]
    
    
    [(hash-table ('nodetype "Compare" )
                 ('left left)
                 ('ops ops)
                 ('comparators comparators))
     
     (PyCompare (get-structured-python left)
                (map get-structured-python ops)
                (map get-structured-python comparators))]
    
    [(hash-table ('nodetype "NotEq"))
     'noteq]
    
    [(hash-table ('nodetype "Lt"))
     'lt]
    
    [(hash-table ('nodetype "Gt"))
     'gt]
    
    [(hash-table ('nodetype "LtE"))
     'lte]
    
    [(hash-table ('nodetype "GtE"))
     'gte]
    
    [(hash-table ('nodetype "Eq"))
     'eq]
    
    [(hash-table ('nodetype "USub"))
     'usub]
    
    [(hash-table ('nodetype "Dict" )
                 ('values values)
                 ('keys keys))     
     (PyDict (map get-structured-python keys)
             (map get-structured-python values))]
    
    [(hash-table ('nodetype "Lambda" )
                 ('args args)
                 ('body body))
     (PyLambda (  get-structured-python args)
               (get-structured-python body))]
    
    [(hash-table ('nodetype "arguments" )
                 ('args args)
                 ('defaults  defaults)
                 ('kwargannotation  kwargannotation)
                 ('vararg  vararg)
                 ('kwarg kwarg)
                 ('varargannotation varargannotation)
                 ('kw_defaults  kw_defaults)
                 ('kwonlyargs kwonlyargs))
     (map get-structured-python args)]
    
    [(hash-table ('nodetype "arg" )
                 ('arg  arg )
                 ('annotation annotation))
     (string->symbol  arg)]
    
    [(hash-table ('nodetype "Assign" )
                 ('value  value )
                 ('targets targets))
     (PySet! (map PyId-x (map get-structured-python targets)) (get-structured-python value) )]
    
    
    [(hash-table ('nodetype "FunctionDef" )
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list decorator_list)
                 ('returns returns))
     (PyFunc (string->symbol name)
             (get-structured-python args)
             (map get-structured-python body))]
    
    [(hash-table ('nodetype "Return" )
                 ('value  value))
     (cond
       [(equal? #\nul  value)
        (PyReturn (PyId 'None))]
       [else (PyReturn (get-structured-python value))])];TODO a null????
    
    
    [(hash-table ('nodetype "ClassDef" )
                 ('name name)
                 ('body body)
                 ('decorator_list decorator_list)
                 ('keywords keywords)
                 ('kwargs kwargs)
                 ('starargs starargs)
                 ('bases bases))
     (PyClass (string->symbol name)
              (map get-structured-python body))]
    
    [(hash-table ('nodetype "Attribute" )
                 ('attr attr)
                 ('value value)
                 ('ctx ctx))
     (PyDot (get-structured-python value) (string->symbol attr))]
    
    
    
    
    
    
    [_ (error 'parse pyjson)]))



