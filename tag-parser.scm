(load "qq.scm")


(define Error
  (lambda (sexpr)
      (syntax-error sexpr "fail")))


(define Quasiquote
  (lambda (sexpr)
    (parse (expand-qq (cadr sexpr)))))


(define conds?
	(lambda (lst)
		(fold-left (lambda (x y) (and x (list? y) (or (= (length y) 2) (> (length y) 2)))) #t lst)
		))


(define Cond
    (lambda (sexpr)
         (if (equal? (car (cadr sexpr)) 'else)
             (parse `(begin ,@(cdr (cadr sexpr))))
             (if (equal? (length (cdr sexpr)) 1)
                 (parse `(if ,(car (cadr sexpr)) (begin ,@(cdr (cadr sexpr)))))
                 (parse `(if ,(car (cadr sexpr))  (begin ,@(cdr (cadr sexpr))) (cond ,@(cddr sexpr))))))))


(define And
    (lambda (sexpr)
	 (cond
           ((equal? (length sexpr) 1)
            (parse '#t))
           ((if (= (length (cdr sexpr)) 1)
                (parse (car (cdr sexpr)))
                (if (= (length (cdr sexpr)) 2)
                    (parse `(if ,(car (cdr sexpr)) ,(car (cdr (cdr sexpr))) #f))
                    (parse `(if ,(car (cdr sexpr)) (and . ,(cdr (cdr sexpr))) #f))))))))
          

(define vars?
	(lambda (sexpr)
		(fold-left (lambda (x y) (and x (list? y) (= (length y) 2))) #t sexpr)
		))

(define Letrec
    (lambda (sexpr)
        (cond
          ((null? (cadr sexpr))
       (parse `(let () (let () ,(cadr (cdr sexpr))))))
      ((and (list? (cadr sexpr)) (vars? (cadr sexpr)))
       (parse `(let
                   ,(map (lambda (var) (list (car var) #f)) (cadr sexpr))
                 ,@(map (lambda (var) (list 'set! (car var) (cadr var))) (cadr sexpr))
                 (let () ,@(cdr (cdr sexpr))))))
      (else (Error sexpr)))))



(define Let*
  (lambda (sexpr)
    (cond
      ((null? (cadr sexpr))
       (parse `(let () ,@(cddr sexpr))))
      ((var? (car (car (cadr sexpr))))
       (if (null? (cdr (cadr sexpr)))
       (parse `(let ((,(car (car (cadr sexpr))) ,(cadr (car (cadr sexpr))))) ,@(cdr (cdr sexpr))))
       (parse `(let ((,(car (car (cadr sexpr))) ,(cadr (car (cadr sexpr))))) (let* ,(cdr (cadr sexpr)) ,@(cdr (cdr sexpr)))))))
      (else (Error sexpr)))))


(define takevals
    (lambda (sexpr)
	 (map cadr sexpr)))

(define takevars
     (lambda (sexpr)
         (map car sexpr)))


(define Let
    (lambda (sexpr)
        (cond
          ((and (list? (cadr sexpr)) (null? (cadr sexpr)))
           (parse `((lambda  ,(takevars (cadr sexpr))  ,@(cddr sexpr)) ,@(takevals (cadr sexpr)))))   
          ((and (list? (cadr sexpr)) (fold-left (lambda (x y) (and x (list? y) (= (length y) 2))) #t (cadr sexpr)))
           (parse `((lambda  ,(takevars (cadr sexpr))  ,@(cddr sexpr)) ,@(takevals (cadr sexpr)))))
          (else (Error sexpr)))))


(define applications
    (lambda (sexpr)
        `(applic ,(parse (car sexpr)) ,(map parse (cdr sexpr)))))


(define assignments
    (lambda (sexpr)
      `(set ,(parse (car (cdr sexpr))) ,@(map parse (cddr sexpr)))))


(define nestedBegin
    (lambda (sexpr)
        (fold-left (lambda (x y) (append x 
        		(if (and (list? y) (eq? (car y) 'begin)) (nestedBegin (cdr y)) (list y))
        	)) '() sexpr)
        ))

(define Begin
  (lambda (sexpr)
      (if (= (length sexpr) 1)
          `(const ,(void))
          (if (= (length (cdr sexpr)) 1)
              (parse (car (cdr sexpr)))
              `(seq ,(map parse (nestedBegin (cdr sexpr))))))))
  


(define var?
  (lambda (sexpr)
    (and (symbol? sexpr)
          (not (member sexpr '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))))))

          
(define duplicatecheck
     (lambda (lst)
         (if (null? lst) #t
       	    (and (not (memq (car lst) (cdr lst))) (duplicatecheck (cdr lst))))))


(define impprotopro
  (lambda (seq)
       (if (list? seq) seq
    (if (pair? seq) (cons (car seq) (impprotopro (cdr seq))) (cons seq '())))
	))


(define Define
    (lambda (sexpr)
      (cond
        ;regular define
        ((var? (cadr sexpr))      
          `(define ,(parse (cadr sexpr)) ,(parse `(begin ,@(cdr (cdr sexpr))))))
        ;mit define
         ((and
;          (equal? (length sexpr) 3)
          (pair? (cadr sexpr))
          (fold-left (lambda (x y) (and x (var? y))) #t (impprotopro (cadr sexpr)))
           (duplicatecheck (impprotopro (cadr sexpr))))
          `(define ,(parse (car (cadr sexpr))) ,(parse `(lambda ,(cdr (cadr sexpr)) ,@(cddr sexpr)))))     
        (else (Error sexpr)))))

          
(define Lambda
    (lambda (sexpr)
      (cond
        ;regular lambda
        ((and
          ;args check
          (list? (cadr sexpr)) (duplicatecheck (impprotopro (cadr sexpr))) (fold-left (lambda (x y) (and x (var? y))) #t (cadr sexpr))
          ;body check
             (list? (cdr (cdr sexpr))) (not (null? (cdr (cdr sexpr)))))
               `(lambda-simple . (,(cadr sexpr) ,(parse `(begin ,@(cddr sexpr))))))
        ;optional lambda
            ((and
          ;args check
          (pair? (cadr sexpr))
           (fold-left (lambda (x y) (and x (var? y))) #t (impprotopro (cadr sexpr)))
           (duplicatecheck (impprotopro (cadr sexpr)))
          ;body check
             (list? (cdr (cdr sexpr)))
             (not (null? (cdr (cdr sexpr)))))
               `(lambda-opt ,(reverse (cdr (reverse (impprotopro (cadr sexpr))))) ,(car (reverse (impprotopro (cadr sexpr)))) ,(parse `(begin ,@(cddr sexpr)))))
           ;variadic lambda
          ((and
          ;args check
          (var? (cadr sexpr))
          ;body check
             (list? (cdr (cdr sexpr)))
             (not (null? (cdr (cdr sexpr)))))
               `(lambda-opt () ,(cadr sexpr) ,(parse `(begin ,@(cddr sexpr)))))
        (else (Error sexpr)))))


(define disjunctions
    (lambda (sexpr)
      (cond
        ;empty or 
        ((equal? (length sexpr) 1)
        `(const ,#f))
        ((equal? (length sexpr) 2)
         (parse (cadr sexpr)))
        ;or with params
        (else
        `(or ,(map parse (cdr sexpr)))))))


(define conditionals
    (lambda (sexpr)
      (cond
        ;if3
        ((equal? (length sexpr) 4)
          `(if3 ,(parse (cadr sexpr)) ,(parse (cadr (cdr sexpr))) ,(parse (cadr (cdr (cdr sexpr))))))
        ;if2
        ((equal? (length sexpr) 3)
          `(if3 ,(parse (cadr sexpr)) ,(parse (cadr (cdr sexpr))) (const ,(void))))
        (else (Error sexpr)))))


(define variables
      (lambda (variable) `(var ,variable)))


(define constants
  (lambda (sexpr)
    (if (and (list? sexpr) (not (null? sexpr)))
        `(const ,(cadr sexpr))
        `(const ,sexpr))))

 

(define parse
  (lambda (sexpr)
  (cond ((var? sexpr)
       (variables sexpr))
      ((or (null? sexpr) (vector? sexpr) (boolean? sexpr) (char? sexpr) (number? sexpr) (string? sexpr) (quote? sexpr)) 
       (constants sexpr))
      ((and (list? sexpr) (equal? 'if (car sexpr)))
       (conditionals sexpr))
      ((and (list? sexpr) (equal? 'or (car sexpr)))
       (disjunctions sexpr))
      ((and (list? sexpr) (equal? 'lambda (car sexpr)) (> (length sexpr) 2))
       (Lambda sexpr))
      ((and (list? sexpr) (equal? 'define (car sexpr)) (> (length sexpr) 2))
       (Define sexpr))
      ((and (list? sexpr) (equal? 'set! (car sexpr)) (> (length sexpr) 2) (var? (cadr sexpr)))
       (assignments sexpr))
      ((and (list? sexpr) (not (member (car sexpr) '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))))
       (applications sexpr))  
      ((and (list? sexpr) (equal? 'let (car sexpr)) (> (length sexpr) 2))
       (Let sexpr))
      ((and (list? sexpr) (equal? 'let* (car sexpr)) (> (length sexpr) 2))
       (Let* sexpr))
      ((and (list? sexpr) (equal? 'letrec (car sexpr)) (> (length sexpr) 2))
       (Letrec sexpr))
      ((and (list? sexpr) (equal? 'begin (car sexpr)))
       (Begin sexpr))
      ((and (list? sexpr) (equal? 'and (car sexpr)))
       (And sexpr))
      ((and (list? sexpr) (equal? 'cond (car sexpr)) (not (null? (cdr sexpr))) (conds? (cdr sexpr))) 
       (Cond sexpr))
      ((and (list? sexpr) (equal? 'quasiquote (car sexpr)) (> (length sexpr) 1))
       (Quasiquote sexpr))
      (else
       (Error sexpr))
       ))
  )

