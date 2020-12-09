;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname interpreter_stacked_scopes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;constructor
(define empty-scope
  (lambda () (list 'empty-scope)))

;adds name-value to current environment
(define extend-scope
  (lambda (var-name var-value scope)
    (cond
      ((empty-scope? scope) (list (list 'extend-scope var-name var-value) scope))
      (else (cons (list 'extend-scope var-name var-value) scope)))))

;builds multiple name-value pairs onto an env at once
(define extend-scope*
  (lambda (lon lov scope)
    (cond
      ((null? lon) scope)
      (else (extend-scope* (cdr lon) (cdr lov) (extend-scope (car lon) (car lov) scope)))))) 

;is this env empty?
(define empty-scope?
  (lambda (scope)
    (eq? (car scope) 'empty-scope))) 

;is this env not empty?[dead code]
;(define extend-env?
  ;(lambda (env)
    ;(eq? (car env) 'extend-env))) 

;scope getters
;return variable name
(define get-name 
  (lambda (scope)
    (cadr scope)))

;return variable value
(define get-value
  (lambda (scope)
    (caddr scope)))

;returns everything except top scope 
(define get-scope
  (lambda (env)
    (cdr env)))

;returns value associated  w/ var-name or #f if not found
(define apply-scope
  (lambda (var-name scope)
    (cond
      ((empty-scope? scope) #f)                                   ;if empty, resolve to false
      ((eq? var-name (get-name scope)) (get-value scope))
      (else #f))))

;returns top-scope on top of stack-of-scopes[dead code][doesn't matter because ]
(define peek-env
  (lambda (env)
    (if (null? env)
        #f
        (car env))))

;returns everything except top scope
(define remove-scope
  (lambda (env)
    (cdr env)))

(define push-scope
  (lambda (scope env)  ;env = stack-of-scopes   they're the same thing
    (cons scope env)))


;is a variable name already used? 
;true when bound(does it have binding? no)
;false when not bound
(define has-binding?
  (lambda (var-name scope)
    (cond
      ((empty-scope? scope) #f)
      ((eq? var-name (get-name scope)) #t)
      (else #f))))


;empty stack constructor
;stack of scopes
(define empty-env
  (lambda () '((empty-scope))))  

;takes name-value pair and makes it a scope, then attaches it to env 
(define extend-env
  (lambda (var-name var-value env)
    (push-scope (list 'extend-scope var-name var-value) env))) 

                     

(define apply-env
  (lambda (var-name env)
    (cond
      ((null? env) #f) 
      ((has-binding? var-name (peek-env env)) (apply-scope var-name (peek-env env))) 
      (else (apply-env var-name (remove-scope env))))))     




;(define env (extend-env 'a 5 (extend-env 'b 7 (empty-env))))
;(extend-env* '(c d e) '(1 2 3) env)


;Grammar constructors
(define var-exp
  (lambda (s)
    (list 'var-exp s)))

(define lambda-exp
  (lambda (s lc-exp)
    (list 'lambda-exp s lc-exp)))

(define app-exp
  (lambda (lambda-exp param-value)
    (list 'app-exp lambda-exp param-value))) ;lambda-exp = operator      param-value=operand

(define lit-exp
  (lambda (n)
    (list 'lit-exp n)))

(define math-exp
  (lambda (operator left right)
    (list 'math-exp operator left right)))

(define bool-exp
  (lambda (operator left right)
    (list 'bool-exp operator left right)))

(define if-exp
  (lambda (bool-exp true-exp false-exp)
    (list 'if-exp bool-exp true-exp false-exp)))

(define print-exp
  (lambda (exp)
    (list 'print-exp exp)))

(define batch-exp   ;takes in lo-exp("list of expressions")
  (lambda (lo-exp)
    (cons 'batch-exp lo-exp)))

(define create-var-exp
  (lambda (var-name var-val)
    (list 'create-var-exp var-name var-val)))

;grammar getters
(define lc-exp->type ;get type of lambda calculus expression(var, lambda, app)
  (lambda (lc-exp)
    (car lc-exp)))
;***************************

;lit-exp
(define lit-exp->value
  (lambda (lit-exp)
    (cadr lit-exp)))
;***************************

;math-exp
(define math-exp->operator
  (lambda (math-exp)
    (cadr math-exp)))

(define math-exp->left
  (lambda (math-exp)
    (caddr math-exp)))

(define math-exp->right
  (lambda (math-exp)
    (cadddr math-exp)))
;***************************

;bool-exp

(define bool-exp->operator
  (lambda (bool-exp)
    (cadr bool-exp)))

(define bool-exp->left
  (lambda (bool-exp)
    (caddr bool-exp)))

(define bool-exp->right
  (lambda (bool-exp)
    (cadddr bool-exp)))
;***************************

;if-exp
(define if-exp->bool-exp
  (lambda (if-exp)
    (cadr if-exp)))

(define if-exp->true-exp
  (lambda (if-exp)
    (caddr if-exp)))

(define if-exp->false-exp
  (lambda (if-exp)
    (cadddr if-exp)))
;***************************

;print-exp
(define print-exp->exp
  (lambda (print-exp)
    (cadr print-exp)))

;***************************

;batch-exp
(define batch-exp->lo-exp
  (lambda (batch-exp)
    (cdr batch-exp)))

;***************************

;create-var-exp

(define create-var-exp->var-name
  (lambda (create-var-exp)
    (cadr create-var-exp)))

(define create-var-exp->var-val
  (lambda (create-var-exp)
    (caddr create-var-exp)))

;***************************


;var-exp
(define var-exp->var-name 
  (lambda (var-exp)
    (cadr var-exp)))
;***************************
;lambda-exp
(define lambda-exp->parameter-name
  (lambda (lambda-exp)
    (cadr lambda-exp)))

(define lambda-exp->body
  (lambda (lambda-exp)
    (caddr lambda-exp)))
;***************************
;app-exp
(define app-exp->lambda-exp
  (lambda (app-exp)
    (cadr app-exp)))

(define app-exp->parameter-input
  (lambda (app-exp)
    (caddr app-exp)))


;Grammar predicates
(define var-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'var-exp)))

(define lambda-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'lambda-exp)))

(define app-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'app-exp)))

(define lit-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'lit-exp)))

(define math-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'math-exp)))

(define bool-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'bool-exp)))

(define if-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'if-exp)))

(define print-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'print-exp)))

(define batch-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'batch-exp)))

(define create-var-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'create-var-exp)))

;Codex code stuff here!!!!
;(get-value 'a)

;(func 'gets x 'does y)          [y is also codex, needs to be parsed]

;(run (func 'gets x 'does y) 'with parameter)

;(literal 5)

;(domath x +,-,*,/ y)

;(test (get-value a) operator (literal 7))

;(ask-question (test (get-value a) operator (literal 7)) if-true-do-> (get-value a) if-false-do-> (get-value a)

;(display (literal 7))

;(do-in-order (literal 7) (literal 7) (literal 7))

;(remember a 13)

;(update a (literal 13))

;Codex getters!!!!! not to be confused w/ scheme getters
(define literal-exp->value
  (lambda (literal-exp)
    (cadr literal-exp)))

(define remember-exp->var-name
  (lambda (remember-exp)
    (cadr remember-exp)))

(define remember-exp->var-val
  (lambda (remember-exp)
    (caddr remember-exp)))

(define do-in-order-exp->lo-exp
  (lambda (do-in-order-exp)
    (cdr do-in-order-exp)))

(define display-exp->exp
  (lambda (display-exp)
    (cadr display-exp)))

(define ask-question->test-exp
  (lambda (question-exp)
    (cadr question-exp)))

(define ask-question->true-exp
  (lambda (question-exp)
    (cadddr question-exp)))

(define ask-question->false-exp
  (lambda (question-exp)
    (car (cdr (cdr (cdr (cdr (cdr question-exp))))))))

(define test-exp->operator
  (lambda (test-exp)
    (caddr test-exp)))

(define test-exp->left
  (lambda (test-exp)
    (cadr test-exp)))

(define test-exp->right
  (lambda (test-exp)
    (cadddr test-exp)))

(define domath-exp->operator
  (lambda (domath-exp)
    (caddr domath-exp)))

(define domath-exp->left
  (lambda (domath-exp)
    (cadr domath-exp)))

(define domath-exp->right
  (lambda (domath-exp)
    (cadddr domath-exp)))

(define get-value-exp->value
  (lambda (get-value-exp)
    (cadr get-value-exp)))

(define func-exp->value
  (lambda (func-exp)
    (caddr func-exp)))

(define func-exp->body
  (lambda (func-exp)
    (car (cdr (cdr (cdr (cdr func-exp)))))))

(define run-exp->func
  (lambda (run-exp)
    (cadr run-exp)))

(define run-exp->parameter
  (lambda (run-exp)
    (cadddr run-exp))) 


;math helper
(define math-machine
  (lambda (operator left-value right-value)
    (cond
      ((eq? operator '+) (+ left-value right-value))
      ((eq? operator '-) (- left-value right-value))
      ((eq? operator '*) (* left-value right-value))
      ((eq? operator '/) (/ left-value right-value))
      (else #f))))

;bool helper
(define boolean-machine
  (lambda (op left right)
    (cond
      ((eq? op '<) (< left right))
      ((eq? op '<=) (< left right))
      ((eq? op '>) (> left right))
      ((eq? op '>=) (>= left right))
      ((eq? op '==) (= left right))
      ((eq? op '!=) (not (= left right)))))) 

;Parse/Unparse
(define parse ;parse-expression
  (lambda (codex)
    (cond
      ((eq? (car codex) 'domath) (math-exp ;constructor
                                (domath-exp->operator codex) ;operator
                                (parse (domath-exp->left codex))  ;first exp/num 
                                (parse (domath-exp->right codex)))) ;second exp/num
      ((eq? (car codex) 'literal) (lit-exp (literal-exp->value codex)))
      ((eq? (car codex) 'test) (bool-exp 
                                   (test-exp->operator codex)
                                   (parse (test-exp->left codex))
                                   (parse (test-exp->right codex))))
      ((eq? (car codex) 'ask-question) (if-exp
                                        (parse (ask-question->test-exp codex))
                                        (parse (ask-question->true-exp codex))
                                        (parse (ask-question->false-exp codex))))
      ((eq? (car codex) 'do-in-order) (batch-exp (map parse (do-in-order-exp->lo-exp codex))))
      ((eq? (car codex) 'display) (print-exp (parse (display-exp->exp codex))))
      ((eq? (car codex) 'remember) (create-var-exp
                                    (remember-exp->var-name codex)
                                    (parse (remember-exp->var-val codex))))  
      ((eq? (car codex) 'get-value) (var-exp
                                     (get-value-exp->value codex))) ;var-exp value
      ((eq? (car codex) 'func) (lambda-exp
                                (func-exp->value codex)             ;lambda-exp value
                                (parse (func-exp->body codex))))    ;lambda-exp body
      ((eq? (car codex) 'run) (app-exp
                               (parse (run-exp->func codex))          ;app-exp lambda
                               (parse (run-exp->parameter codex))))
      ;((eq? (car codex) 'repeat) (repeat-exp (parse (repeat-exp->times codex))
      ;                                       (parse (repeat-exp->what codex))))

      )));app-exp param name      



(define apply-expression-to-list
  (lambda (lo-parsed-exp env)
    (cond
      ((null? lo-parsed-exp) '())
      ((create-var-exp? (car lo-parsed-exp)) (apply-expression-to-list          ;if it's a parsed remember,
                                        (cdr lo-parsed-exp)                     ;apply next expression
                                        (apply-expression (car lo-parsed-exp) env)));using applied remember, update the environment
      (else (cons (apply-expression (car lo-parsed-exp) env)                    ;otherwise, go ahead and resolve the exp in the current env
              (apply-expression-to-list (cdr lo-parsed-exp) env))))))  

;take parsed code and execute it
(define apply-expression
  (lambda (lc-exp env)
    (cond
      ((math-exp? lc-exp) (math-machine
                           (math-exp->operator lc-exp)
                           (apply-expression (math-exp->left lc-exp) env)   ;since both are lc-exp, the math machine needs the actual #s not the exp, so we run apply
                           (apply-expression (math-exp->right lc-exp) env)))
      ((bool-exp? lc-exp) (let ((op (bool-exp->operator lc-exp))
                                (left (apply-expression (bool-exp->left lc-exp) env))
                                (right (apply-expression (bool-exp->right lc-exp) env)))
                            (boolean-machine op left right)))
      ((if-exp? lc-exp) (if (apply-expression (if-exp->bool-exp lc-exp) env)
                            (apply-expression (if-exp->true-exp lc-exp) env)
                            (apply-expression (if-exp->false-exp lc-exp) env)))
      ((print-exp? lc-exp) (write (apply-expression (print-exp->exp lc-exp) env)))
      ((batch-exp? lc-exp) (apply-expression-to-list (batch-exp->lo-exp lc-exp) env)) 
      ((create-var-exp? lc-exp) (extend-env
                                 (remember-exp->var-name lc-exp)
                                 (apply-expression (remember-exp->var-val lc-exp) env)
                                 env))
      ((lit-exp? lc-exp) (lit-exp->value lc-exp)) 
      ((var-exp? lc-exp) (apply-env (var-exp->var-name lc-exp) env))                           ;get variable name from var-exp, look up value with apply-env, resolve value
      ((lambda-exp? lc-exp) (apply-expression (lambda-exp->body lc-exp) env))                  ;execute body of lambda-exp, which is a lc-exp, so call apply on it
      ((app-exp? lc-exp) (let* ((the-lambda (app-exp->lambda-exp lc-exp))                      ;let creates a local env
                                (the-lambda-param-name (lambda-exp->parameter-name the-lambda));3 variable here: 
                                (the-parameter-value (apply-expression (app-exp->parameter-input lc-exp) env))       ;(lambda-exp from app-exp),(name of param in lambda-exp), (new value for that param)
                                (the-new-env (extend-env the-lambda-param-name the-parameter-value env)))  ;update env with above variables
                           (apply-expression the-lambda the-new-env))))))   

(define run-program
  (lambda (codex-src env)
    (apply-expression (parse codex-src) env))) 







;test code here
;(define env (extend-env 'a 7 (extend-env 'b 9 (empty-env)))) 
;env
;(define codex-example '(run (func gets a does (get-value a)) with (get-value b)))
;(parse codex-example)
;(run-program codex-example env)



;(define test-env (extend-env* '(c d e) '(1 2 3) (empty-env)))
;(apply-expression (var-exp 'c) test-env)
;(run-program codex-example test-env)

;(define codextest '(get-value c))
;(run-program codextest test-env)

;(define codextest2 '(literal 7))
;(run-program codextest2 test-env)

;(define math-codex '(domath (get-value c) / (run (func gets a does (get-value a)) with (literal 7))))
;(parse math-codex)
;(run-program math-codex test-env)

;(define bool-codex '(test (get-value c) > (literal 7)))
;(define BoolCodex '(test (literal 5) != (literal 6))) 
;(run-program BoolCodex test-env)

;(define if-codex '(ask-question
                   ;(test (literal 75) == (literal 7))
                                ;if-true-do-> (literal 1)
                                ;if-false-do-> (literal 0)))
;(run-program if-codex test-env)

;(define display-codex '(display (literal 7)))
;(parse display-codex)

;(run-program display-codex test-env)

;(parse '(do-in-order (literal 7) (literal 8) (remember a (literal 13))))
;(run-program '(remember a (literal 13)) test-env)


;(run-program '(do-in-order (literal 7) (literal 8) (get-value c)) test-env)

;(parse '(do-in-order (remember t (literal 3)) (get-value t)))
;(run-program '(do-in-order (remember t (literal 5)) (get-value t)) test-env)









;new test code

(define env (extend-env 'b 3 (extend-env 'a 7 (empty-env))))
;env
;(apply-env 'c env)

;(define math-codex '(domath (literal 7) + (get-value b)))
;(parse math-codex)
;(run-program math-codex env)

;(define codex-run '(run (func gets a does (literal 6)) with (get-value b)))
;(parse codex-run)
;(run-program codex-run env)


;(define codex-test '(test (get-value a) == (literal 7)))
;(parse codex-test)
;(run-program codex-test env)

;(define codex-conditional '(ask-question (test (get-value a) == (literal 7)) 
				;if-true-do-> (literal 1) 
				;if-false-do-> (literal 0)))
;(run-program codex-conditional env)

;(define codex-literal '(display (literal 7)))
;(run-program codex-literal env)

;(define codex-order '(do-in-order (get-value a)(literal 7) (literal 8) (literal 9)))
;(parse codex-order)
;(run-program codex-order env)

(define codex-remember '(remember c (literal 13)))

(parse codex-remember)

(run-program codex-remember env)








 
      









     