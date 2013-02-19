#lang racket/base

(require (planet dherman/json:4:0))
(require racket/cmdline
         racket/pretty
         "parse-python.rkt"
         "get-structured-python.rkt"
         "python-interp.rkt"
         "python-desugar.rkt"
         "python-lib.rkt"
         "run-tests.rkt"
         "python-evaluator.rkt")

(define (python-test-runner _ port)
  (run-python port))

(define (run-python port)
  (wrap-interp
   (interp
    (python-lib
     (desugar-wrap
      (get-structured-python
       (parse-python/port port python-path)))))))

(define (run-python-desugar port)
  (display
  ; (python-lib
     (desugar-wrap
      (get-structured-python
       (parse-python/port port python-path))))) 

(define (run-python-surface port)
  (display
      (get-structured-python
       (parse-python/port port python-path)))) 



(define (run-python-lib port)
  (display
   (python-lib
     (desugar-wrap
      (get-structured-python
       (parse-python/port port python-path)))))) 


(define python-path ;"/home/joe/bin/python")
 "/course/cs173/python/Python-3.2.3/python")
;	"/usr/local/bin/python3")
  

(command-line
  #:once-each
  ("--interp" "Interpret stdin as python"
   (run-python (current-input-port)))

  ("--d" "Interpret stdin as python"
   (run-python-desugar (current-input-port)))

  
  ("--l" "Interpret stdin as python"
   (run-python-lib (current-input-port)))

  ("--s" "Interpret stdin as python"
   (run-python-surface (current-input-port)))

  
  
  ("--interp-py" "Interpret stdin as python using py-prelude.py"
   (define results ((mk-python-cmdline-eval python-path) "stdin" (current-input-port)))
   (display (car results) (current-output-port))
   (display (cdr results) (current-error-port)))

  ("--get-syntax" "Get s-exp for python"
   (pretty-write (parse-python/port (current-input-port) python-path)))

  ("--test" dirname "Run all tests in dirname"
   (display (results-summary (run-tests (mk-proc-eval/silent python-test-runner) dirname))))

  ("--test-py" dirname "Run all tests in dirname using python"
   (display (results-summary (run-tests (mk-python-cmdline-eval python-path) dirname))))

  ("--python-path" path "Set the python path"
   (set! python-path path))

  ("--progress-report" dirname "Generate a soft report"
   (printf "~a\n"
    (jsexpr->json
     (json-summary
      (run-tests (mk-proc-eval/silent python-test-runner) dirname)))))
)

