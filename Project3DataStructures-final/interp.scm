(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
 
  



 
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
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
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))



        ;;array
	   (newarray-exp (count-exp val-exp)
			 (let ((count (expval->num (value-of count-exp env)))
			       (val (value-of val-exp env)))
			   (array-val (make-array count val))))

	   (read-array-exp (exp1 exp2)
			 (let ((v1 (value-of exp1 env))
			       (v2 (value-of exp2 env)))
			   (let ((p (expval->array v1))
				 (pos (expval->num v2)))
			     (array-at p pos))))

	   (update-array-exp (exp1 exp2 exp3)
			 (let ((v1 (value-of exp1 env))
			       (v2 (value-of exp2 env))
			       (v3 (value-of exp3 env)))
			   (let ((p (expval->array v1))
				 (pos (expval->num v2)))
			     (array-set! p pos v3))))

          (arraylength-exp (exp)
			    (let ((arr (expval->array
					(value-of exp env))))
			      (num-val (array-length arr))))

      


        ;;stack
          (newstack-exp ()
              (let ((stack-array (make-array 1001 #f)))
             (begin
               (array-set! stack-array 0 (num-val 0))
               (array-val stack-array))
           ))
                      
        
          (stack-push-exp (exp1 exp2)
             (let ((v1 (value-of exp1 env))
                   (value (value-of exp2 env)))
	       (let ((stack-array (expval->array v1)))     
                (let ((counter (expval->num (array-at stack-array 0))))
                  (begin 
                    (array-set! stack-array 0 (num-val (+ 1 counter)))
                    (array-set! stack-array (+ 1 counter) value))))))
        
          

        (stack-size-exp (exp1)
		(let ((stack-array (expval->array (value-of exp1 env))))
			       (array-size stack-array)))

         (empty-stack-exp (exp1)
           (let ((stack-array (expval->array (value-of exp1 env))))
             (bool-val (empty-array? stack-array)))) 

         (stack-pop-exp (exp1)
			 (let ((v1 (value-of exp1 env)))
			   (let ((stack-array (expval->array v1)))
                             (if (empty-array? stack-array)
                                 (num-val -1)
                                 (let ((counter (expval->num (array-at stack-array 0))))
                                   (let ((pop-val (array-at stack-array counter)))
                                     (begin
                                        (array-set! stack-array 0 (num-val (- counter 1)))
                                        (array-set! stack-array counter (bool-val #f))
                                        pop-val)))))))                            
                                
       (stack-top-exp (exp1)
          (let ((v1 (value-of exp1 env)))
	   (let ((stack-array (expval->array v1)))
             (if (empty-array? stack-array)
                 (num-val -1)
                 (let ((counter (expval->num (array-at stack-array 0))))
                   (let ((top-val (array-at stack-array counter)))
                     top-val))))))

        (print-stack-exp (exp1)
          	 (let ((v1 (value-of exp1 env)))
		   (let ((stack-array (expval->array v1)))
                      (if (empty-array? stack-array)
                      (display "Stack is empty!")
                      (print-array stack-array 1))))) 

        ;;queue
         (newqueue-exp ()
           (let ((queue-array (make-array 1003 #f)))
             (begin
               (array-set! queue-array 0 (num-val 0))
               (array-set! queue-array 1001 (num-val 1))
               (array-set! queue-array 1002 (num-val 1))
               (array-val queue-array))
           ))         

          (queue-push-exp (exp1 exp2)
           (let ((queue-array (expval->array (value-of exp1 env)))
                 (value (value-of exp2 env)))
             (let ((counter (expval->num (array-at queue-array 0)))
                   (tail (expval->num (array-at queue-array 1002))))
               (begin 
                 (array-set! queue-array 0 (num-val (+ 1 counter)))
                 (array-set! queue-array 1002 (num-val (+ 1 tail)))
                 (array-set! queue-array tail value)))))  

         (queue-size-exp (exp1)
           (let ((queue-array (expval->array (value-of exp1 env))))
             (array-size queue-array)))

         (empty-queue-exp (exp1)
           (let ((queue-array (expval->array (value-of exp1 env))))
             (bool-val (empty-array? queue-array))))    

         (queue-pop-exp (exp1)
           (let ((queue-array (expval->array (value-of exp1 env))))
             (if (empty-array? queue-array)
                 (num-val -1)
                 (let ((counter (expval->num (array-at queue-array 0)))
                       (header (expval->num (array-at queue-array 1001))))
                   (let ((pop-val (array-at queue-array header)))
                     (begin
                       (array-set! queue-array 0 (num-val (- counter 1)))
                       (array-set! queue-array 1001 (num-val (+ header 1)))
                       (array-set! queue-array header (bool-val #f))                       
                       pop-val))))))

         (queue-top-exp (exp1)
           (let ((queue-array (expval->array (value-of exp1 env))))
             (if (empty-array? queue-array)
                 (num-val -1)
                 (let ((header (expval->num (array-at queue-array 1001))))
                   (let ((top-val (array-at queue-array header)))
                     top-val)))))

        (print-queue-exp (exp1)
           (let ((queue-array (expval->array (value-of exp1 env))))
             (if (empty-array? queue-array)
                 (display "Queue is empty!")
                 (print-array queue-array 1))))
			     
           )))
       


;array
  
(define make-array
  (lambda (count value)
    (letrec ((do-alloc
	      (lambda (count)
		(if (> count 0)
		    (let ((new (newref value)))
		      (do-alloc (- count 1)))
                       (eopl:printf "")
                    ))))
      (let ((header (newref value)))
	(do-alloc (- count 1))
	(array-v header count)))))

;; index check for array
(define array-chk
  (lambda (arr pos)
    (cases array arr
	   (array-v (header count)
		    (if (>= pos count)
	         	(eopl:printf "")
			#t)))))
(define array-at
  (lambda (arr pos)
    (if (array-chk arr pos)
	(cases array arr
	       (array-v (header count)
			(deref (+ header pos))))
         (eopl:printf "")
        )))

(define array-set!
  (lambda (arr pos val)
    (if (array-chk arr pos)
	(cases array arr
	       (array-v (header count)
			(setref! (+ header pos) val)))
         (eopl:printf "")
        )))

(define array-length
  (lambda (arr)
    (cases array arr
	   (array-v (header count)
		    count))))

 
  (define array-size
    (lambda (arr)
   (array-at arr 0)))
    
  (define empty-array?
    (lambda (arr)
       (= (expval->num (array-size arr)) 0)))

  (define print-array
    (lambda (arr n)
      (cases expval (array-at arr n)
        (bool-val (bool) (display "\nTop"))
        (num-val (num) (begin (display "\n") (display num) (print-array arr (+ n 1))))
        (else (display "ERROR: UNEXPECTED ELEMENT"))
        )))
 
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
