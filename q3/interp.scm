(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)


;; parser cleanup

 (define (args-list a)
   (cases args a
     (many-args (id1 exp1 ids exps)
          (cons
            (cons id1 ids)
            (cons exp1 exps)))
     (no-args () (cons '() '()))
       ))
 
;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
  
  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (args body)
          (letrec
              ((pair (args-list args))
               (names (car pair))
               (defualts (actualize-args (cdr pair) env))
               (new-env (apply-args names defualts env))
               )
          (proc-val (procedure names body new-env))))

        (call-exp (rator args)
          (letrec
              (
                (proc (expval->proc (value-of rator env)))
               (pair (args-list args))
               (names (car pair))
               (vals (actualize-args (cdr pair) env))
               )
            (apply-procedure proc names vals)))

        )))
  ;;
  (define (apply-args names args env)
      (if (null? names) env
          ( apply-args
            (cdr names)
            (cdr args)
            (extend-env (car names) (car args) env) )
          )
    )
  
  (define (actualize-args val-exps env)
    (map (lambda (x)  (value-of x env) ) val-exps)
   )

  (define (verify-names given existing)
    (if (null? given) given
        (begin
          (if (not(member (car given) existing))
              (eopl:error 'apply-env "~s is not an arg" (car given))
              (verify-names (cdr given) existing)
           )
          given
     ))
  )
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 names vals)
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of body
                    (apply-args
                       (verify-names names vars)
                       vals saved-env))))))

  )
