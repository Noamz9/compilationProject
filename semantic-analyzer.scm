
(define get-params
    (lambda (exp)
        (if (eq? (car exp) 'lambda-simple)
           (cadr exp)
	   (append (cadr exp) (list (caddr exp)))))
  )


(define get-body
   (lambda (exp)
       (if (eq? (car exp) 'lambda-simple)
	(caddr exp)
	(cadddr exp)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define remove-applic-lambda-nil
  (lambda (pe)
    (cond 
      ((and (list? pe) (not (null? pe)) (equal? 'applic (car pe)) (list? (cadr pe)) (equal? 'lambda-simple (caadr pe)) (null? (cadadr pe)) (null? (caddr pe)))
       (remove-applic-lambda-nil (caddr (cadr pe))))
      ((list? pe)
       (map remove-applic-lambda-nil pe))
      (else
       pe)
       ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      



(define box-set
  (lambda (exp)
    (cond
      ((and (list? exp) (not (null? exp)) (equal? 'lambda-simple (car exp)))
       `(lambda-simple ,(cadr exp) ,(define-box (cadr exp) (box-set (caddr exp)))))
      ((and (list? exp) (not (null? exp)) (equal? 'lambda-opt (car exp)))
       `(lambda-opt ,(cadr exp) ,(caddr exp) ,(define-box (append (cadr exp) (cons (caddr exp) '())) (box-set (cadddr exp)))))
      ((list? exp)
       (map box-set exp))
      (else
       exp)))
  )





(define define-box
   (lambda (params body)
	(let ((sets (make-set-box-list params body))
	    (body (body-update params body)))
		(if (null? sets)
                body
		(if (and (list? body) (eq? (car body) 'seq))		
		    `(seq (,@sets ,@(cadr body)))
                    `(seq (,@sets ,body))))
			))
  )





(define make-set-box-list
    (lambda (params body)
        (if (null? params)
          '()
          (if (and (bound-in-body? (car params) body) (has-get-occurrence? (car params) body) (has-set-occurrence? (car params) body))
	      (cons `(set (var ,(car params)) (box (var ,(car params)))) (make-set-box-list (cdr params) body))
	      (make-set-box-list (cdr params) body))))
  )





(define body-update
    (lambda (params body)
	(if (null? params)
           body
	   (if (and (bound-in-body? (car params) body) (has-get-occurrence? (car params) body) (has-set-occurrence? (car params) body))
	       (body-update (cdr params) (box-update (car params) body))
	       (body-update (cdr params) body))))
               
  )





(define box-update
    (lambda (var exp)
      (cond
        ((equal? exp `(var ,var))
         `(box-get (var ,var)))
        ((and (list? exp) (not (null? exp)) (equal? (car exp) 'set) (list? (cadr exp)) (equal? (cadadr exp) var))
         `(box-set (var ,var) ,(box-update var (caddr exp))))
        ((or (not (list? exp)) (null? exp))
         exp)
        ((or (and (eq? (car exp) 'lambda-simple) (member var (get-params exp))) 
             (and (eq? (car exp) 'lambda-opt) (member var (get-params exp))))
         exp)
        (else
         (map (lambda (x) (box-update var x)) exp))))
  )





(define bound-in-body?
    (lambda (var body)
        (if (not (list? body))
          #f
          (if (eq? (car body) 'seq)
	      (ormap (lambda (exp) (bound-in-exp? var exp)) body)
	      (bound-in-exp? var body))))
  )





(define bound-in-exp?
    (lambda (var exp)
        (if (and (list? exp) (not (null? exp)))
            (if (or (and (eq? (car exp) 'lambda-simple) (not (member var (get-params exp)))) 
		    (and (eq? (car exp) 'lambda-opt) (not (member var (get-params exp)))))
                (if (not (list? (caddr exp)))
                    (eq? (caddr exp) var)
		    (ormap (exists-in-any-level? var) exp))
		(if (or (eq? (car exp) 'lambda-simple) (eq? (car exp) 'lambda-opt))
		    #f 
		    (ormap (lambda (x) (bound-in-exp? var x)) exp)))
	     #f))
  )





(define exists-in-any-level?
   (lambda (var)
      (lambda (exp)
	 (cond
           ((or (not (list? exp)) (and (list? exp) (null? exp)))
            #f)
	   ((equal? exp `(var ,var))
            #t)		
	   ((or (eq? (car exp) 'lambda-simple) (eq? (car exp) 'lambda-opt))
	    (bound-in-exp? var exp))
           (else
            (ormap (exists-in-any-level? var) exp)))))
  )





(define has-get-occurrence?
    (lambda (var body)
        (if (list? body)
            (if (equal? (car body) 'seq)
                (ormap (get-in-any-level? var) body)
                ((get-in-any-level? var) body))
             #f))
  )





(define get-in-any-level?
    (lambda (var)
	(lambda (exp)
          (cond 
            ((or (not (list? exp)) (and (list? exp) (null? exp)))
             #f)
            ((equal? exp `(var ,var))
             #t)
            ((and (list? exp) (eq? (car exp) 'set))
             (if ((get-in-any-level? var) (caddr exp))
                 #t
                 #f))
            ((or (and (eq? (car exp) 'lambda-simple) (member var (get-params exp))) 
	         (and (eq? (car exp) 'lambda-opt) (member var (get-params exp))))
             #f)
            (else
             (ormap (get-in-any-level? var) exp)))))
  )





(define has-set-occurrence?
    (lambda (var body)
        (if (list? body)
            (if (equal? (car body) 'seq)
                (ormap (set-in-any-level? var) body)
                ((set-in-any-level? var) body))
            #f))
  )





(define set-in-any-level?
    (lambda (var)
	(lambda (exp)
           (cond 
             ((or (not (list? exp)) (and (list? exp) (null? exp)))
              #f)
             ((and (list? exp) (equal? (car exp) 'set) (list? (cadr exp)) (equal? (cadadr exp) var))
              #t)
             ((or (and (eq? (car exp) 'lambda-simple) (member var (get-params exp))) 
		  (and (eq? (car exp) 'lambda-opt) (member var (get-params exp))))
              #f)
             (else
              (ormap (set-in-any-level? var) exp)))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define pe->lex-pe
    (lambda (exp)
      (analyze-lex exp '() #f))
  )





(define analyze-lex
    (lambda (exp scope params)
	(if (and (list? exp) (not (null? exp)))
	    (let ((new-scope (if (or (eq? (car exp) 'lambda-simple)
		                     (eq? (car exp) 'lambda-opt))
                                 (if (not (list? params))
                                     scope
                                     (append (list params) scope))
                                 scope))
		  (new-params (if (or (eq? (car exp) 'lambda-simple)
				      (eq? (car exp) 'lambda-opt))
                                  (get-params exp)
                                  params)))
		    (if (eq? (car exp) 'var)
			(let ((index-of-parameter (search-in-params (cadr exp) new-params 0))
			      (index-of-binding (search-in-scope (cadr exp) new-scope 0)))
				(if (not (= index-of-parameter -1))
                                    `(pvar ,(cadr exp) ,index-of-parameter)
				    (if (list? index-of-binding)
                                        `(bvar ,(cadr exp) ,(car index-of-binding) ,(cadr index-of-binding))
				        `(fvar ,(cadr exp)))))
		        (map (lambda (x) (analyze-lex x new-scope new-params)) exp)))
              exp))
  )





(define search-in-scope
    (lambda (var scope index)
         (if (not (null? scope))
	     (let ((minor (search-in-params var (car scope) 0)))
		(if (= minor -1)
                    (search-in-scope var (cdr scope) (+ 1 index))
		    (list index minor)))
             -1))
  )





(define search-in-params
    (lambda (var params index)
	 (if (and  (list? params) (not (null? params)))
	     (if (eq? (car params) var)
                 index
		 (search-in-params var (cdr params) (+ 1 index)))
             -1))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define annotate-tc
  (lambda (exp) 
    (in-tp exp #f))
  )





(define in-tp
  (lambda (exp tp?)
    (cond
      ((or (not (list? exp)) (null? exp) (eq? (car exp) 'pvar) (eq? (car exp) 'fvar) (eq? (car exp) 'bvar) (eq? (car exp) 'const))
	exp)
      ((eq? (car exp) 'applic)
       (if tp?
           `(tc-applic ,(annotate-tc (cadr exp)) ,(map annotate-tc (caddr exp)))
	   `(applic ,(annotate-tc (cadr exp)) ,(map annotate-tc (caddr exp)))))
      ((eq? (car exp) 'if3)
       `(if3 ,(annotate-tc (cadr exp)) ,(in-tp (caddr exp) tp?) ,(in-tp (cadddr exp) tp?)))
      ((eq? (car exp) 'or)
       `(or (,@(map annotate-tc (reverse (cdr (reverse (cadr exp))))) ,(in-tp (car (reverse (cadr exp))) tp?))))
      ((eq? (car exp) 'def)
       `(def ,(cadr exp) ,(annotate-tc (caddr exp))))
      ((eq? (car exp) 'lambda-simple)
       `(,(car exp) ,(cadr exp) ,(in-tp (get-body exp) #t)))
      ((eq? (car exp) 'lambda-opt)
       `(,(car exp) ,(cadr exp) ,(caddr exp) ,(in-tp (get-body exp) #t)))
      ((or (eq? (car exp) 'set) (eq? (car exp) 'box-set))
       `(,(car exp) ,(cadr exp) ,(annotate-tc (caddr exp))))
      ((eq? (car exp) 'seq)
       `(seq (,@(map annotate-tc (reverse (cdr (reverse (cadr exp))))) ,(in-tp (car (reverse (cadr exp))) tp?))))
      (else
       (map (lambda (x) (in-tp x tp?)) exp))))
  )
