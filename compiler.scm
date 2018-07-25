(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")

;(define my-map "(define map (lambda (func lst) (if (null? lst) lst (cons (func (car lst)) ;(map func (cdr lst))))))\n")

(define my-map"(define map
  ((lambda (y) 
     ((lambda (map1) 
	((lambda (maplist) 
	   (lambda (f . s) 
	     (maplist f s))) 
	 (y (lambda (maplist) 
	      (lambda (f s) 
		(if (null? (car s)) '() 
		    (cons (apply f (map1 car s)) 
			  (maplist f (map1 cdr s))))))))) 
      (y (lambda (map1) 
	   (lambda (f s) 
	     (if (null? s) '() 
		 (cons (f (car s)) 
		       (map1 f (cdr s))))))))) 
   (lambda (f) 
     ((lambda (x) 
	(f (lambda (y z)
	     ((x x) y z))))
      (lambda (x) 
	(f (lambda (y z)
	     ((x x) y z))))))))\n")


(define my-list "(define list (lambda x x))\n")
(define my-fold-left "(define fold-left (lambda (func acc lst) (if (null? lst) acc (fold-left func (func acc (car lst)) (cdr lst)))))\n")
(define my-append-2 "(define append-2 (lambda (ls1 ls2) (if (null? ls1) ls2 (cons (car ls1) (append-2 (cdr ls1) ls2)))))\n")
(define my-append "(define append (lambda x (fold-left append-2 '() x)))\n")


(define pipeline
  (lambda(s)
    ((star <sexpr>) s
     (lambda(m r)
      (map (lambda (e)
             (annotate-tc
              (pe->lex-pe
               (box-set
                (remove-applic-lambda-nil
                 (parse e))))))
           m))
     (lambda (f) 'fail))))


(define file->list
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
(run)))))



(define run-time-support
  (lambda (lst)
    (string->list (string-append my-map my-list my-fold-left my-append-2 my-append (list->string lst) 
                                 ))))



(define compile-scheme-file
  (lambda (source-file target-file)
    (let* ((parsed-exps (pipeline (run-time-support (file->list source-file))))
            (const-table (hash-label-const (construct-const-table parsed-exps) 0))
            (fvar-table (hash-label-free-vars (construct-fvar-table parsed-exps (compute-last-index const-table)) 0))
            (prologue  (list->string (file->list "scheme.s")))
            )
      ;(display const-table)
      ;(display parsed-exps)
       (string->file
        target-file
	(string-append
         prologue
         (make-label-const const-table const-table)
         (make-label-free-vars fvar-table)
         (code-gen-symbol-table const-table 0 (count-strings const-table))
         "\nsection .text \nmain:\n push rbp\n"
         (code-gen-apply fvar-table const-table)
         (code-gen-< fvar-table const-table)
         (code-gen-= fvar-table const-table)
         (code-gen-> fvar-table const-table)
         (code-gen-plus fvar-table const-table)
         (code-gen-div fvar-table const-table)
         (code-gen-multi fvar-table const-table)
         (code-gen-minus fvar-table const-table)
         (code-gen-boolean? fvar-table const-table)
         (code-gen-car fvar-table const-table)
         (code-gen-cdr fvar-table const-table)
         (code-gen-char->integer fvar-table const-table)
         (code-gen-char? fvar-table const-table)
         (code-gen-cons fvar-table const-table)
         (code-gen-denominator fvar-table const-table)
         (code-gen-eq? fvar-table const-table)
         (code-gen-integer? fvar-table const-table)
         (code-gen-integer->char fvar-table const-table)
         (code-gen-make-string fvar-table const-table)
         (code-gen-make-vector fvar-table const-table)
         (code-gen-not fvar-table const-table)         
         (code-gen-null? fvar-table const-table)       
         (code-gen-number? fvar-table const-table)
         (code-gen-numerator fvar-table const-table)
         (code-gen-pair? fvar-table const-table)
         (code-gen-procedure? fvar-table const-table)
         (code-gen-rational? fvar-table const-table)
         (code-gen-remainder fvar-table const-table)
         (code-gen-string-length fvar-table const-table)
         (code-gen-string-ref fvar-table const-table)
         (code-gen-string-set! fvar-table const-table)
         (code-gen-string->symbol fvar-table const-table)        
         (code-gen-string? fvar-table const-table)
         (code-gen-symbol? fvar-table const-table)
         (code-gen-symbol->string fvar-table const-table)        
         (code-gen-vector fvar-table const-table)
         (code-gen-vector-length fvar-table const-table)        
         (code-gen-vector-ref fvar-table const-table)
         (code-gen-vector-set! fvar-table const-table)       
         (code-gen-vector? fvar-table const-table)
         (code-gen-zero? fvar-table const-table)
         (fold-left (lambda (str parsed-exp) (string-append str (code-gen parsed-exp fvar-table const-table  0) "push qword[rax] \ncall write_sob_if_not_void \nadd rsp, 1*8 \n")) "" parsed-exps)
         "ret\n"
       ))
       )))


(define string->file
  (lambda (target assm-str)
     (begin 
     (delete-file target)
        (let* ((target-port (open-output-file target)))
            (begin (for-each (lambda(ch) (write-char ch target-port)) (string->list assm-str)) 
        (close-output-port target-port))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; const-table

(define construct-const-table
  (lambda (p-exp)
    (let ((const-table `((1 ,(void) T_VOID) (2 () (T_NIL)) (3 ,#f (T_BOOL 0)) (5 ,#t (T_BOOL 1))))
          (const-list (remove-duplicates (expand-const-list (find-consts p-exp) '()) '())))
      (letrec ((helper 
                (lambda (table lst memo)
                  (if (null? lst) table
                      (let ((current (car lst)))
                        (if (or (null? current) (boolean? current) (eq? current (void))) (helper table (cdr lst) memo)
                        (if (integer? current) (helper (append table `((,memo ,current (T_INTEGER ,current)))) (cdr lst) (+ 2 memo))
			(if (char? current) (helper (append table `((,memo ,current (T_CHAR ,(char->integer current))))) (cdr lst) (+ 2 memo))
                        (if (number? current) (helper (append table `((,memo ,current (T_FRACTION ,(look-up-table (numerator current) table) ,(look-up-table (denominator current) table))))) (cdr lst) (+ 3 memo))
                        (if (string? current) (helper (append table `((,memo ,current (T_STRING ,(length(string->list current)) ,@(map char->integer (string->list current)))))) (cdr lst) (+ 2 (length(string->list current)) memo))
                        (if (symbol? current) 
                            (let ((look-up-ans (look-up-table (symbol->string current) table)))
				(if (> look-up-ans -1)
                                    (helper (append table `((,memo ,current (T_SYMBOL ,look-up-ans)))) (cdr lst) (+ 2 memo))
                                    (helper (append table `((,memo ,(symbol->string current) (T_STRING ,(length(string->list (symbol->string current))) ,@(map char->integer (string->list (symbol->string current))))))
					`((,(+ 2 (length(string->list (symbol->string current))) memo) ,current (T_SYMBOL ,memo)))) 
					(cdr lst) (+ 4 (length(string->list (symbol->string current))) memo))))
                        (if (vector? current) (helper (append table `((,memo ,current (T_VECTOR  ,(length (vector->list current)) ,@(map (lambda (x) (look-up-table x table)) (vector->list current)))))) (cdr lst) (+ 2 (length (vector->list current)) memo))
                            (helper (append table `((,memo ,current (T_PAIR  ,(look-up-table (car current) table) ,(look-up-table (cdr current) table))))) (cdr lst) (+ 3 memo))))))))))))))
        
			(helper const-table const-list 7)))))


(define find-consts
  (lambda (exps)
    (fold-left 
     (lambda (acc exp) 
	(if (and (list? exp) (not (null? exp)))
            (if (eq? (car exp) 'const)
                (if (rational? (cadr exp)) (append acc (list (numerator (cadr exp))) (list (denominator (cadr exp))) (list (cadr exp))) 
                    (append acc (list (cadr exp))))  
                
		(append acc (find-consts exp)))
      acc))
      '() exps)))


(define expand-const-list
  (lambda (lst ans)
    (if (null? lst)
	ans
	(if (pair? (car lst))
            (expand-const-list (cdr lst) (append ans (find-inside-const (car lst) '()) (list (car lst)))) 
            (if (vector? (car lst))
                (expand-const-list (cdr lst) (append ans (expand-const-list (vector->list (car lst)) '()) (list (car lst))))
		(expand-const-list (cdr lst) (append ans (list (car lst)))))))))




(define find-inside-const
  (lambda (lst ans)
    (if (null? lst)
        ans
        (if (not (pair? (cdr lst)))
            (if (not (pair? (car lst)))
		(if (vector? (car lst))
                    (append ans (expand-const-list (vector->list (car lst)) '()) (list (car lst)) (list (cdr lst)))
                    (append ans (list (car lst)) (list (cdr lst))))
		(append ans (find-inside-const (car lst) '()) (list (car lst)) (list (cdr lst))))
            (if (not (pair? (car lst)))
		(if (vector? (car lst))
                    (append ans (expand-const-list (vector->list (car lst)) '()) (list (car lst)) (find-inside-const (cdr lst) '()) (list (cdr lst)))
                    (append ans (list (car lst)) (find-inside-const (cdr lst) '()) (list (cdr lst))))
		(append ans  (find-inside-const (car lst) '()) (list (car lst)) (find-inside-const (cdr lst) '()) (list (cdr lst))))))))





(define remove-duplicates
  (lambda (lst ans)
    (if (null? lst)
	ans
	(if (member (car lst) ans)
            (remove-duplicates (cdr lst) ans)
            (remove-duplicates (cdr lst) (append ans (list (car lst))))))))



(define look-up-table
  (lambda (element table)
      ;(newline)(display "look up:  ")(display element)(newline)
      ;(newline)(display "table:  ")(display table)(newline)
    (if (null? table)
        -1
	(if (equal? (cadar table) element)
            (caar table)
            (look-up-table element (cdr table))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fvar-table

(define look-up-for-fvar
  (lambda (element table)
    (if (null? table)
        -1
	(if (equal? (caddar table) element)
            (caar table)
            (look-up-for-fvar element (cdr table))))))


(define hash-label-free-vars
  (lambda (table index)
    (if (null? table)
         '()
         `((,(string-append "Lglob" (number->string index)) ,@(car table)) ,@(hash-label-free-vars (cdr table) (+ 1 index)))
         )))


(define make-label-free-vars
  (lambda (table)
    ;(newline)(display "make-label-const: ")(display table)(newline)
    (if (null? table)
        ""
        (string-append (caar table) ":\n\t dq SOB_UNDEFINED\n" (make-label-free-vars (cdr table))))))
      


(define compute-last-index
  (lambda (memory)
    (let ((temp (reverse memory)))
	(+ (cadar temp) (length (cadddr (car temp)))))))



(define construct-fvar-table
  (lambda (p-exp memo)
    (let ((flst (remove-duplicates (find-free p-exp) '(/ > < = eq? map fold-left list append append-2 string->symbol apply string-set! string-ref vector-set! vector-ref vector boolean? cdr car null? integer->char vector-length string-length char? string? vector? symbol? pair? integer? procedure? cons char->integer numerator denominator number? rational? zero? not remainder set-car! set-cdr! make-vector make-string symbol->string + - *))))
	(letrec ((helper (lambda (lst indx)
                           (if (null? lst)
                               '()
                               (cons (list indx (car lst)) (helper (cdr lst) (+ 1 indx)))))))
          (helper flst memo)))))


(define find-free
  (lambda (exps)
    (fold-left 
	(lambda (acc exp) 
          (if (and (list? exp) (not (null? exp)))
              (if (eq? (car exp) 'fvar)
		(append acc (list (cadr exp)))
		(append acc (find-free exp)))
              acc))
        '() exps)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;const-labels 


(define hash-label-const
  (lambda (table index)
    (if (null? table)
         '()
         `((,(string-append "const" (number->string index)) ,@(car table)) ,@(hash-label-const (cdr table) (+ 1 index)))
         )))


(define make-label-const
  (lambda (table org-table)
     ; (newline)(display "make-label-const: ")(display table)(newline)

    (if (not (null? table))
       (cond
         ((equal? (cadddr (car table)) 'T_VOID)
          (string-append (caar table) ":\n\t dq MAKE_LITERAL(" (symbol->string (cadddr (car table))) ", 0)\n" (make-label-const (cdr table) org-table)))
         ((equal? (caaddr (cdar table)) 'T_NIL)
          (string-append (caar table) ":\n\t dq MAKE_LITERAL(" (symbol->string (caaddr (cdar table))) ", 0)\n" (make-label-const (cdr table) org-table)))
         ((or (equal? (caaddr (cdar table)) 'T_BOOL) (equal? (caaddr (cdar table)) 'T_INTEGER) (equal? (caaddr (cdar table)) 'T_CHAR))
          (string-append (caar table) ":\n\t dq MAKE_LITERAL(" (symbol->string (caaddr (cdar table))) "," (number->string (cadadr (cddar table))) ")\n" (make-label-const (cdr table) org-table)))
         ((equal? (caaddr (cdar table)) 'T_PAIR)
          (string-append (caar table) ":\n\t dq MAKE_LITERAL_PAIR(" (look-up-for-label (cadar (cdddar table)) org-table) ", " (look-up-for-label (caddar (cdddar table)) org-table) ")\n" (make-label-const (cdr table) org-table)))
         ((equal? (caaddr (cdar table)) 'T_STRING)
          (string-append (caar table) ":\n\t MAKE_LITERAL_STRING " (create-new-string (caddar table) "\"" #f) "\n" (make-label-const (cdr table) org-table)))
         ((equal? (caaddr (cdar table)) 'T_VECTOR)
          (if (= (cadadr (cddar table)) 0)
          (string-append (caar table) ":\n\t dq MAKE_LITERAL(T_VECTOR, 0)\n" (make-label-const (cdr table) org-table))
          (string-append (caar table) ":\n\t MAKE_LITERAL_VECTOR " (look-up-for-label-vector (cddar (cdddar table)) org-table) "\n" (make-label-const (cdr table) org-table))))
         ((equal? (caaddr (cdar table)) 'T_FRACTION)
          (string-append (caar table) ":\n\t dq MAKE_LITERAL_FRACTION(" (look-up-for-label (cadadr (cddar table)) org-table) "," (look-up-for-label (caddar (cdddar table)) org-table) ")\n" (make-label-const (cdr table) org-table))) 
         ((equal? (caaddr (cdar table)) 'T_SYMBOL)
          (string-append (caar table) ":\n\t dq MAKE_LITERAL_SYMBOL(" (look-up-for-label (cadar (cdddar table)) org-table)")\n" (make-label-const (cdr table) org-table)))
         (else
             ""))
       "")))


(define look-up-for-label
  (lambda (address table)
    (if (null? table) ""
	(if (equal? address (cadar table))
            (caar table)
            (look-up-for-label address (cdr table))))))


(define look-up-for-label2
  (lambda (value table)
    (if (null? table) ""
	(if (equal? value (caddar table))
            (caar table)
            (look-up-for-label2 value (cdr table))))))


(define look-up-for-label-vector
  (lambda (lst table)
    (if (equal? (length lst) 1)
	(look-up-for-label (car lst) table)
        (string-append (look-up-for-label (car lst) table) ", " (look-up-for-label-vector (cdr lst) table)))))


(define create-new-string
  (lambda (str acc last_special)
    (if (= (string-length str) 0) 
      (if last_special
          acc
          (string-append acc "\""))
      (cond ((eq? (string-ref str 0) #\nul)
                  (create-new-string (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_NUL")
                                                                                       (string-append acc "\""  ", CHAR_NUL")) #t))
            ((eq? (string-ref str 0) #\tab)
                  (create-new-string (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_TAB")
                                                                                       (string-append acc "\""  ", CHAR_TAB")) #t))
            ((eq? (string-ref str 0) #\newline)
                  (create-new-string (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_NEWLINE")
                                                                                       (string-append acc "\""  ", CHAR_NEWLINE")) #t))
            ((eq? (string-ref str 0) #\page)
                  (create-new-string (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_PAGE")
                                                                                       (string-append acc "\""  ", CHAR_PAGE")) #t))
            ((eq? (string-ref str 0) #\return)
                  (create-new-string (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_RETURN")
                                                                                       (string-append acc "\""  ", CHAR_RETURN")) #t))
            ((eq? (string-ref str 0) #\space)
                  (create-new-string (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_SPACE")
                                                                                       (string-append acc "\""  ", CHAR_SPACE")) #t))

          (else (create-new-string (substring str 1 (string-length str)) (if last_special (string-append acc ", \""(substring str 0 1))
                                                                                     (string-append acc (substring str 0 1))) #f))))


    ))
           


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;symbol-table


(define code-gen-symbol-table 
        (lambda (const-table index remaining-symbols)
            (if (= remaining-symbols 0) (if (= index 0) (string-append "symbol_start: \n\t dq const1\n") (string-append "symbol_start: \n\t dq symbol0\n"))
                (if (list? (car (cdddar const-table)))
                    (if (not (equal? (caar (cdddar const-table)) 'T_SYMBOL))
                        (code-gen-symbol-table (cdr const-table) index remaining-symbols)
                        (if (= remaining-symbols 1)
                            (string-append "symbol" (number->string index) ": \n\t dq MAKE_LITERAL_PAIR(" (caar const-table)   ", const1)\n" (code-gen-symbol-table (cdr const-table) (+ index 1) (- remaining-symbols 1)))
                            (string-append "symbol" (number->string index) ": \n\t dq MAKE_LITERAL_PAIR(" (caar const-table)   ", symbol"(number->string (+ index 1)) ")\n"
                                           (code-gen-symbol-table (cdr const-table) (+ index 1) (- remaining-symbols 1)))))
                    (code-gen-symbol-table (cdr const-table) index remaining-symbols)))))





(define count-strings
  (lambda (const-table)
    (if (null? const-table) 0
        (if (list? (car (cdddar const-table)))            
            (if (equal? (caar (cdddar const-table)) 'T_SYMBOL)
                (+ 1 (count-strings (cdr const-table)))
                (count-strings (cdr const-table)))
            (count-strings (cdr const-table))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;code-gens


(define code-gen
  (lambda (parsed-exps fvar-table const-table major)
    (cond
      ((equal? (car parsed-exps) 'const)
       (code-gen-const parsed-exps const-table  major))
      ((equal? (car parsed-exps) 'if3)
       (code-gen-if parsed-exps fvar-table const-table  major))
      ((equal? (car parsed-exps) 'or)
       (code-gen-or parsed-exps fvar-table const-table  major))
      ((equal? (car parsed-exps) 'seq)
       (code-gen-seq (cadr parsed-exps) fvar-table const-table major))
      ((equal? (car parsed-exps) 'define)
       (code-gen-define parsed-exps fvar-table const-table major))
      ((equal? (car parsed-exps) 'lambda-simple)
       (code-gen-lambda-simple parsed-exps fvar-table const-table major))
      ((equal? (car parsed-exps) 'lambda-opt)
       (code-gen-lambda-opt parsed-exps fvar-table const-table major))
      ((equal? (car parsed-exps) 'applic)
       (code-gen-applic parsed-exps fvar-table const-table major))
      ((equal? (car parsed-exps) 'tc-applic)
      (code-gen-tc-applic parsed-exps fvar-table const-table major))   
      ((equal? (car parsed-exps) 'pvar)
       (code-gen-pvar parsed-exps fvar-table const-table ))
      ((and (equal? (car parsed-exps) 'set) (list? (cadr parsed-exps)) (equal? (caadr parsed-exps) 'pvar))
       (code-gen-set-pvar parsed-exps fvar-table const-table (caddr (cadr parsed-exps))))
      ((equal? (car parsed-exps) 'bvar)
       (code-gen-bvar parsed-exps fvar-table const-table (caddr parsed-exps)(cadddr parsed-exps)))
      ((and (equal? (car parsed-exps) 'set) (list? (cadr parsed-exps)) (equal? (caadr parsed-exps) 'bvar))
       (code-gen-set-bvar parsed-exps fvar-table const-table ))
      ((equal? (car parsed-exps) 'fvar)
       (code-gen-fvar parsed-exps fvar-table const-table))
     ; ((and (equal? (car parsed-exps) 'set) (list? (cadr parsed-exps)) (equal? (caadr parsed-exps) 'fvar))
     ;  (code-gen-set-fvar exp fvar-table const-table major))
      ((equal? (car parsed-exps) 'box)
       (code-gen-box parsed-exps fvar-table const-table major))
      ((equal? (car parsed-exps) 'box-get)
        (code-gen-box-get parsed-exps fvar-table const-table major))
      ((equal? (car parsed-exps) 'box-set)
        (code-gen-box-set parsed-exps fvar-table const-table major))  
      (else
       ""))))
        
       
(define code-gen-const
  (lambda (parsed-exps const-table major)
    (string-append "mov rax, " (look-up-for-label2 (cadr parsed-exps) const-table) "\n" )))

(define code-gen-if
  (lambda (parsed-exps fvar-table const-table major)
    (let ((else_label (ifelse_label))
          (exit_label (ifexit_label)))
      (string-append (code-gen (cadr parsed-exps) fvar-table const-table  major) "cmp rax , const2 \nje " else_label "\n"
                     (code-gen (caddr parsed-exps) fvar-table const-table  major) "jmp " exit_label "\n" else_label
                     ":\n" (code-gen (cadddr parsed-exps) fvar-table const-table  major) exit_label ":\n"))))



(define code-gen-or
  (lambda (parsed-exps fvar-table const-table major)
    (let ((exit_label (or_exit_label)))
      (letrec ((helper (lambda (ex ftab ctab maj)
                         (if (null? ex) (string-append exit_label ":\n")
                             (string-append (code-gen (car ex) ftab ctab maj)
                                            (if (> (length ex) 1) (string-append "cmp rax, const2 ;\njne " exit_label "\n") "")
                                            (helper (cdr ex) ftab ctab maj))))))
        (helper (cadr parsed-exps) fvar-table const-table major)
        ))))


(define code-gen-seq
  (lambda (exp fvar-table const-table major)
    (if (null? exp)
        ""
        (string-append (code-gen (car exp) fvar-table const-table major) (code-gen-seq (cdr exp) fvar-table const-table major)))))




(define code-gen-define
  (lambda (exp fvar-table const-table major)
    (string-append
     (code-gen (caddr exp) fvar-table const-table major)
     "mov [" (look-up-for-fvar (cadadr exp) fvar-table) "], rax\n"
     "mov rax, const0\n"
     )))


(define code-gen-lambda-general
  (lambda (body name fvar-table const-table)
    (let* ((clos_body (clos_body_label))
           (clos_exit (clos_exit_label))
           )
      (string-append 
       "mov rdi, 16\n"         
       "call malloc\n"     
       "mov rbx, 0\n"
       "MAKE_LITERAL_CLOSURE rax, rbx, " clos_body "\n" 
       "jmp " clos_exit "\n"
       clos_body ":\n"
       "push rbp\n"
       "mov rbp, rsp\n"
       body"\n"
       "leave\n"
       "ret\n"
       clos_exit ":\n"
       "mov [" (look-up-for-fvar name fvar-table) "], rax\n\n"
       )
                                 )))



(define code-gen-lambda-simple
  (lambda (parsed-exps fvar-table const-table major)
      (let* ((args (cadr parsed-exps))
             (body (caddr parsed-exps))
             (args-len (length args))
             (for1 (for_label))
             (for2 (for_label))
             (exit1 (forexit_label))
             (exit2 (forexit_label))
             (clos_body (clos_body_label))
             (clos_exit (clos_exit_label)))
        (string-append
         "mov rdi, 16\n"
         
         "call malloc\n"
         "push rax\n"
         (if (= major 0) "mov rbx, 1\n"
             (string-append
              "mov rdi, 8 *" (number->string (+ 1 major)) "\n"
              "call malloc\n"
              "mov rbx, rax\n"
              ))
         (if (= major 0) ""
             (string-append
              "mov rax, [rbp + 3*8]\n"
              "push rax\n"
              "mov r15, 8\n"
              "mul r15\n"
              "mov rdi, rax\n"
              "call malloc\n"
              "mov r13, rax\n"
              "mov r11, 0\n"
              "pop rdi\n"
              for1 ":\n"                     
              "cmp r11 ,rdi\n"
              "jge " exit1 "\n"
              "mov rax,r11\n"
              "mov r15, 8\n"
              "mul r15\n"
              "mov rdx, [rbp + 8*(r11+4)]\n"
              "mov qword [r13 + r11*8], rdx \n"
              "add r11, 1\n"
              "jmp " for1 "\n"
              exit1 ":\n"
              "mov qword[rbx], r13\n"
              ))
         (if (< major 2) ""
             (string-append
              "mov r11, 0\n"
              "mov r12, 1\n"
              for2 ":\n"        
              "cmp r11, " (number->string major) "\n"
              "jge " exit2 "\n"
              "mov rax, [rbp + 8*2]\n"
              "mov r14, qword[rax + r11*8]\n"
              "mov [rbx + r12*8], r14\n"
              "add r11, 1\n"
              "add r12, 1\n"
              "jmp " for2 "\n"
              exit2 ":\n"
              ))
         "pop rax\n"
         "MAKE_LITERAL_CLOSURE rax, rbx, " clos_body "\n" 
         "jmp " clos_exit "\n"
         clos_body ":\n"
         "push rbp\n"
         "mov rbp, rsp\n"
         (code-gen body fvar-table const-table (+ major 1))"\n"
         "pop rbp\n"
         "ret\n"
         clos_exit ":\n"
         
         ))))

         
(define code-gen-lambda-opt
  (lambda(exp fvar-table const-table major)
    (let* ((_lambda_opt_for1 (for_label))
           (_lambda_opt_for2 (for_label))
           (_lambda_opt_body (clos_body_label))
           (_lambda_opt_exit (clos_exit_label))
            (_lambda_opt_loop (for_label))
            (_lambda_opt_fix-stack1 (fix_stack_label))
            (_lambda_opt_fix-stack2 (fix_stack_label))
            (empty_list_params (empty_list_params_label))
            (_lambda_opt_continue_ (continue_label))
            (_lambda_opt_continue2_ (continue_label))
           (body (cadddr exp)))

      (string-append
      _lambda_opt_continue_ ":\n\t"
      "mymalloc 2\n\t"
      "push rax\n\t"
      (if (= 0 major) "mov rbx,0\n\t"
      (string-append
      "mov rdi," (number->string (+ 1 major)) "\n\t"
      "mymalloc rdi\n\t"
      "mov rbx, rax\n\t"))
      
      (if (= 0 major) ""
      (string-append
      "mov rdi, [rbp+3*8]\n\t" 
      "mymalloc rdi\n\t"
        
      "mov r8, rax\n\t"))
      

      (if (= 0 major) ""
      (string-append
      "mov r10,0\n"
      _lambda_opt_for1":\n\t"
      "mov r11,qword[rbp+8*(r10 + 4)]\n\t"
      "mov [r8+r10*8],r11\n\t"
      "inc r10\n\t"
      "cmp r10,rdi\n\t"
      "jl "_lambda_opt_for1"\n\t"
      "mov qword [rbx],r8\n\t")) 

      (if (> 2 major) ""
      (string-append
      "mov r9,1\n\t"
      "mov r10,0\n"            
      _lambda_opt_for2":\n\t"
      "mov rsi, qword[rbp+8*2]\n\t"    
      "mov rsi,[rsi+r10*8]\n\t"    
      "mov [rbx+r9*8],rsi\n\t"     
      "add r10, 1\n\t"
      "add r9, 1\n\t"
      "cmp r9," (number->string major) "\n\t"
      "jl "_lambda_opt_for2"\n\t"))

      "mov r12, "(number->string (length (cadr exp))) "\n\t"
      "pop rax\n\t"
      "MAKE_LITERAL_CLOSURE rax, rbx, "_lambda_opt_body"\n\t"
      "jmp "_lambda_opt_exit"\n"
      _lambda_opt_body":\n\t"
      "push rbp\n\t"
      "mov rbp,rsp\n\t"
      
      "mov r14, [rbp +3*8]\n\t"    
      "mov r13, r14\n\t"            
      "add r13, 3\n\t"
      "shl r13, 3\n\t"            
      "mov r12, rbp\n\t"        
      "add r12, r13\n\t"           
      "sub r14, "(number->string (length (cadr exp)))"\n\t"     
      "cmp r14,0\n\t"
      "je "empty_list_params "\n\t"
      "mov r15 , 0\n\t"
      "mov r13 , [const1]\n"
      _lambda_opt_loop ":\n\t"

      "mymalloc 2\n\t"
      "mov r11, [r12]\n\t"
      "mov r11, [r11]\n\t"
      "mov qword[rax] , r11\n\t"
      "mov qword[rax+8] , r13\n\t"
      "mov r8, rax\n\t"
      "add rax, 8\n\t"
      "mov r13, rax\n\t"

      "MAKE_LITERAL_PAIR2 r8, r13\n\t"
      "mov r13, r8\n\t"
      "add r15, 1\n\t"
      "sub r12, 8\n\t"
      "cmp r15,r14\n\t"
      "jl "_lambda_opt_loop "\n\t"

      "mymalloc 1\n\t"

      "mov qword [rax], r13\n\t"
      "add r12 , 8\n\t" 
      "mov qword [r12], rax \n\t"
      "mov r9 , 1\n\t"
      "add r9 , "(number->string (length (cadr exp))) "\n\t" 
      "mov r14, r12\n\t"
      "mov r12, [rbp+ 3*8]\n\t"
      "mov qword [rbp +3*8] , r9 \n\t"
      "mov r15, r12\n\t" 
      "add r15, 3\n\t"
      "shl r15, 3\n\t"
      "add r15, rbp\n\t"
      "mov r13 , r14\n\t"
      "sub r13 , rbp\n\t"
      "shr r13 , 3\n\t"
      "add r13, 1\n"

      _lambda_opt_fix-stack1":\n\t"
      "mov r11,[r14]\n\t"           
      "mov qword [r15], r11\n\t"
      "sub r14,8\n\t"
      "sub r15,8\n\t"
      "sub r13,1\n\t"
      "cmp r13,0\n\t"
      "ja "_lambda_opt_fix-stack1"\n\t" 

      "sub r15, r14\n\t" 
      "add rsp , r15\n\t"
      "add rbp , r15\n\t"
      "jmp "_lambda_opt_continue2_"\n"

      empty_list_params ":\n\t"
      "mov r13, [rbp +3*8]\n\t"  
      "mov r12, r13\n\t"    
      "add r12,1\n\t"
      "mov qword [rbp+3*8] , r12\n\t"
      "add r13, 3\n\t"
      "add r13, 1\n\t"
      "mov r14, rbp\n\t"
      "mov r15, r14\n\t"
      "sub r15 , 8\n"
      
      _lambda_opt_fix-stack2":\n\t"
      "mov r11,[r14]\n\t"            
      "mov qword [r15], r11\n\t"
      "add r14,8\n\t"
      "add r15,8\n\t"
      "dec r13\n\t"
      "cmp r13,0\n\t"
      
      "ja "_lambda_opt_fix-stack2"\n\t" 
      "sub rsp , 8\n\t"
      "sub rbp , 8\n\t"
      "mov r14,const1\n\t"
      "mov qword [r15], r14\n"

      _lambda_opt_continue2_":\n\t"
      (code-gen body fvar-table const-table (+ 1 major)) "\n\t"
      "pop rbp\n\t"
      "ret\n"
      _lambda_opt_exit":\n"
      ))))    
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         

;; (define code-gen-lambda-opt
;;   (lambda (parsed-exps fvar-table const-table major)
;;     (let* ((num-of-params (length(append (car parsed-exps) (list (cadr parsed-exps)))))
;;            (_lambda_opt_not_first_body (clos_body_label))
;;            (_lambda_opt_first_body (clos_body_label))
;;            (_lambda_opt_major_exit (forexit_label))
;;            (_lambda_opt_minor_exit (forexit_label))
;;            (_lambda_opt_major_loop (for_label))
;;            (_lambda_opt_minor_loop (for_label))
;;            (clos_exit (clos_exit_label))
;;            (clos_body (clos_body_label))
;;            (notFirstLambda
;;             (string-append
;;              _lambda_opt_not_first_body": \n\t"
;;              "mov rdi, 16\n\t"
;;              "call malloc\n\t"                ;rdx=body rbx=major rcx=params
;;              "mov r13,rax\n\t"
;;              "mov rdi, " (number->string (* 8 (+ 1 major))) "\n\t"
;;              "call malloc\n\t"
;;              "mov r14,rax\n\t"
;;              
;;              "mov rcx, [rbp+8*2]\n\t"
;;              
;;              "mov r9," (number->string  (+ 1 major)) "\n\t"
;;              "mov r8, 0\n"
;;              
;;              _lambda_opt_major_loop ": \n\t"
;;              
;;              "cmp r8, r9\n\t"
;;              "je "_lambda_opt_major_exit "\n\t"
;;              "mov r12,r8\n\t"
;;              "shl r12,3\n\t"
;;              "add r12,rcx\n\t"
;;              "mov rdi, qword [r12]\n\t"
;;              "inc r8\n\t"
;;              "mov qword [r14+r8*8],rdi\n\t"
;;              
;;              "jmp "_lambda_opt_major_loop"\n\t"
;;              _lambda_opt_major_exit": \n\t" 
;;              "mov rcx, [rbp+8*3]\n\t"
;;              "shl rcx, 3\n\t"
;;              "mov rdi, rcx\n\t"
;;              "call malloc\n\t"  ;8*r8
;;              "mov rcx,rax\n\t"
;;              "mov rax, [rbp+8*3]\n\t"  
;;              "mov r8,0\n"
;;              
;;              _lambda_opt_minor_loop ": \n\t"
;;              
;;              "cmp r8, rax\n\t"
;;              "je "_lambda_opt_minor_exit"\n\t"
;;              "mov rdi,[rbp+8*(r8+4)]\n\t"
;;              "mov [rcx+8*r8],rdi \n\t" 
;;              "inc r8\n\t" 
;;              
;;              "jmp "_lambda_opt_minor_loop "\n\t"
;;              _lambda_opt_minor_exit": \n\t"
;;              "mov [r14], rcx\n\t" 
;;              "mov rax,r13\n\t" 
;;              "MAKE_LITERAL_CLOSURE rax,r14,"clos_body "\n\t"
;;              "jmp " clos_exit "\n"
;;              
;;              clos_body ": \n\t" 
;;              
;;              "push rbp\n\t"
;;              "mov rbp, rsp\n\t"
;;              (fix-empty-variadic-var num-of-params)
;;              (code-gen (caddr parsed-exps) fvar-table const-table (+ major 1))
;;              "pop rbp\n\t" 
;;              "ret\n\t" 
;;              clos_exit ":"
;;              ))
;;            (firstLambda 
;;             (string-append
;;              _lambda_opt_first_body": \n\t"
;;              "mov rdi, 8\n\t"
;;              "call malloc \n\t"
;;              "mov qword [rax], 0\n\t"
;;              "mov r13, rax\n\t"
;;              "mov rdi, 16\n\t"
;;              "call malloc \n\t"
;;              "MAKE_LITERAL_CLOSURE rax,r13," clos_body "\n\t"
;;              "jmp " clos_exit "\n"
;;              clos_body ": \n\t"
;;              "push rbp\n\t"
;;              "mov rbp, rsp\n\t"
;;              (fix-empty-variadic-var num-of-params)
;;              (code-gen (caddr parsed-exps) fvar-table const-table (+ 1 major)) "\n\t"
;;              "pop rbp\n\t"
;;              "ret\n\t"
;;              
;;              clos_exit": \n")
;;             ))
;;       
;;       (if (= major 0)
;;           firstLambda
;;           notFirstLambda)  
;;       )))
;; 
;; 
;; (define fix-empty-variadic-var	
;;   (lambda (num-of-params)
;;     (let ((clos_exit (clos_exit_label))
;;           (for_label (forexit_label)))
;;       (string-append 
;;        "mov r8, qword [rbp + 3*8]\n\t"
;;        "cmp r8," (number->string num-of-params) "\n\t"
;;        "jle " clos_exit "\n\t"
;;        
;;        "mov r9, r8\n\t" 
;;        "sub r9," (number->string num-of-params) "\n\t"		
;;        "mov r10, 0\n"
;;        for_label   ": \n\t" 
;;        "cmp r10, r9\n\t" 
;;        "je "  clos_exit "\n\t"
;;        "mov r14, r8\n\t" 
;;        "add r14, 3\n\t"
;;        "sal r14, 3\n\t" 				
;;        "add r14, rbp\n\t"  		
;;        "mov r13, qword [r14]\n\t"
;;        "mov rdi, 8\n\t"
;;        "call malloc\n\t" 
;;        "mov qword[rax], r13\n\t" 
;;        "mov r13, rax\n\t" 
;;        "sub r14, 8\n\t"		
;;        "mov r12, qword [r14]\n\t"
;;        "mov rdi, 8\n\t" 
;;        "call malloc\n\t"
;;        "mov qword[rax], r12\n\t" 
;;        "mov r12, rax\n\t"
;;        "mov rdi, 8\n\t" 
;;        "call malloc\n\t"
;;        "MAKE_MALLOC_LITERAL_PAIR rax, r12, r13\n\t"
;;        "mov rax, qword [rax]\n\t" 
;;        "mov qword [r14], rax\n\t"
;;        "dec r8\n\t" 
;;        "inc r10\n\t" 
;;        "jmp " for_label "\n\t"
;;        
;;        clos_exit ":\n\t"
;;        ))))



           


(define code-gen-applic
  (lambda (exp fvar-table const-table major)
    (string-append
     (list-of-strings->string (map (lambda (x) (string-append (code-gen x fvar-table const-table major) "push rax\n")) (reverse (caddr exp))))
     "mov rbx, "(number->string  (length (caddr exp))) "\n"
     "push rbx\n"
     (code-gen (cadr exp) fvar-table const-table major)
     "mov rbx, qword[rax]\n"
     "CLOSURE_ENV rbx\n"
     "push rbx\n"
     "mov rax, qword[rax]\n"
     "CLOSURE_CODE rax\n"
     "call rax\n"          
     "mov rbx, [rsp+8]\n"
     "add rbx,2\n"
     "shl rbx, 3\n"
     "add rsp,rbx\n"
     )
    ))






(define code-gen-tc-applic
  (lambda (exp fvar-table const-table major)
    (let
        ((for1 (for_label))
         (exit1 (forexit_label)))
      (string-append
       (list-of-strings->string (map (lambda (x) (string-append (code-gen x fvar-table const-table major) "push rax\n")) (reverse (caddr exp))))
       "mov rbx, "(number->string (length (caddr exp))) "\n"
       "push rbx\n"
       (code-gen (cadr exp) fvar-table const-table major)
       "mov rbx, qword[rax]\n"
       "CLOSURE_ENV rbx\n"
       "push rbx\n"
       "mov rax, qword[rax]\n"
       "CLOSURE_CODE rax\n"
  
       "mov rbx, [rbp+8]\n"  ;step1
       "push rbx\n"
        
       "mov r11, rbp\n"      ;step2
        
       "mov rbp,[rbp]\n"     ;step3
       "mov r12,[r11+8*3]\n"
       "add r12, 4\n"
       "shl r12,3\n"
       "add r12,r11\n"
       "mov rbx, "(number->string (+ (length (caddr exp)) 3)) "\n"
        for1 " :\n"
       "cmp rbx, 0\n"
       "je " exit1 "\n"
       "sub r11, 8\n"
       "sub r12, 8\n"
       "mov r13, [r11]\n"
       "mov [r12],r13\n"
       "dec rbx\n"
       "jmp "for1 "\n"
        exit1 " :\n" 
       "mov rsp, r12\n"
       "jmp rax\n" 
        ))))




(define code-gen-pvar
  (lambda (exp fvar-table const-table)
    (string-append "mov rax, qword[rbp + (4 +" (number->string (caddr exp)) ")*8]  \n")
    
    ))


(define code-gen-set-pvar
  (lambda (exp fvar-table const-table minor)
    (string-append
     (code-gen (caddr exp) fvar-table const-table 0) "\n"
     "mov [rbp + "(number->string (+ 4 minor)) "*8], rax \n"
     "mov rax, SOB_VOID\n"
     )))


(define code-gen-bvar
  (lambda (exp fvar-table const-table major minor)
    (string-append
     "mov rax, [rbp + 2*8]  \n"
     "mov rax, [rax + " (number->string major) "*8]  \n"
     "mov rax, [rax + "(number->string minor) "*8]  \n"
     )))


(define code-gen-set-bvar
  (lambda (exp fvar-table const-table)
    (string-append
     (code-gen (caddr exp) fvar-table const-table 0) "\n"   
     "mov rbx, qword[rbp + 2*8]  \n"
     "mov rbx, qword[rbx + " (number->string (caddr (cadr exp))) "*8]  \n"
     "mov qword[rbx + " (number->string (cadddr (cadr exp))) "*8], rax \n"
     "mov rax, SOB_VOID\n"
     )))

(define code-gen-fvar
  (lambda (exp fvar-table const-table)
    (string-append
     "mov rax, [" (look-up-for-fvar (cadr exp) fvar-table) "]\n"
     )))


(define code-gen-box
  (lambda (exp fvar-table const-table major)
    (string-append
     (code-gen (cadr exp) fvar-table const-table (caddr (cadr exp))) "\n" 
     "mov rbx,  rax\n"
     "mov rdi, 8\n"
     "call malloc\n"
     "mov qword[rax], rbx\n"
     )))

(define code-gen-box-get
  (lambda (exp fvar-table const-table major)
    (string-append
     (code-gen (cadr exp) fvar-table const-table (caddr (cadr exp))) "\n" 
     "mov rax, [rax]\n"
     )))

(define code-gen-box-set
  (lambda (exp fvar-table const-table major)
		(string-append
                 (code-gen (caddr exp) fvar-table const-table major) "\n" 
                 "mov rbx, rax\n"
                 (code-gen (cadr exp) fvar-table const-table (caddr (cadr exp))) "\n"
                 "mov [rax], rbx\n"
                 "mov rax, SOB_VOID\n"
                 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;run-time support



(define code-gen-apply
  (lambda(fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "apply_body:\n"
      "mov rax, [rbp+4*8]\n"	;procedure			
      "mov rax, qword [rax]\n"
      "mov r10, qword [rbp]\n" ;old rbp
      "mov r11,qword [rbp+8]\n" ;ret addres
      "mov r12, rbp\n"            ;cur rbp  
       "add r12, 6*8\n"
  
      "mov rcx, [rbp+5*8]\n"   ;list
      "mov rcx, qword [rcx]\n" ;
      "mov rbx, rcx\n"
      "TYPE rbx\n"
 
      "mov rsi, 0\n"  ;counter
     
      "apply_calculate_list_length:\n"
      "cmp rbx, T_NIL\n"
      "je apply_calculate_list_length_done\n"
      "CDR rcx\n"
      "mov rbx, rcx\n"
      "TYPE rbx\n"
      "inc rsi\n"
      "jmp apply_calculate_list_length\n"
     
      "apply_calculate_list_length_done:\n"
	        "shl rsi, 3\n"
	        "sub r12, rsi\n"
	        "shr rsi, 3\n"
     
      "mov rdi, 0\n"
      "mov rcx, [rbp+5*8]\n"  	;the list		 
      "mov rcx, qword [rcx]\n"
     
      "apply_loop:\n"
     
      "cmp rdi, rsi\n"
      "je apply_loop_exit\n"
      "mov rbx, rcx\n"
      "DATA_UPPER rbx\n"
      "add rbx, start_of_data\n"    
      "mov qword [r12 + 8*rdi], rbx\n"
      "CDR rcx\n"
      "inc rdi\n"
      "jmp apply_loop\n"
     
      "apply_loop_exit:\n"
     
      "sub r12, 8\n"
      "mov qword [r12],rsi\n" ;save num of args
      "sub r12, 8\n"
      "mov rbx, rax\n"
      "CLOSURE_ENV rbx\n"
      "mov qword [r12], rbx\n" ;save the env
      "sub r12, 8\n"
      "mov qword [r12], r11\n" ;save the ret add   
      "mov rsp, r12\n"
      "mov rbp, r10\n"
	        "mov rbx, rax\n"
	        "TYPE rbx\n"
	        
	        "cmp rbx, T_CLOSURE\n"
	        "jne apply_finish\n"
      
      "CLOSURE_CODE rax\n"
      "jmp rax\n"
      "apply_finish:\n"
     )
    'apply fvar-table const-table)))



(define code-gen-integer->char
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_integer_to_char_body: \n\t"
      "mov rbx, qword[rbp+4*8]\n\t"
      "mov rbx ,[rbx]\n\t"
      "DATA rbx\n\t"
      "shl rbx, TYPE_BITS\n\t"
      "or rbx, T_CHAR\n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov [rax], rbx\n"
      ) 'integer->char fvar-table const-table)))


(define code-gen-char->integer
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_char_to_integer_body: \n\t"
      "mov rbx, qword[rbp+4*8]\n\t"
      "mov rbx ,[rbx]\n\t"
      "DATA rbx\n\t"
      "shl rbx, TYPE_BITS\n\t"
      "or rbx, T_INTEGER\n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov [rax], rbx\n\t"
      ) 'char->integer fvar-table const-table)))

(define code-gen-not
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_not_body: \n\t"
      "mov r14,[rbp+8*(4+0)]\n\t" ;arg
      "mov r14,[r14]\n\t"
      "mov r15,[rbp+8*(3+0)]\n\t" ;arg count
      "cmp r14,SOB_FALSE\n\t"
      "je _not_return_true\n\t"
      "mov rax," (look-up-for-label2 #f const-table)"\n\t"
      "jmp _not_finish\n"
      "_not_return_true:\n\t"
      "mov rax," (look-up-for-label2 #t const-table)"\n"       
      "_not_finish:\n"
      )
     'not fvar-table const-table)))

(define code-gen-null?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_nil_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_NIL\n\t"
      "je _is_nil_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_nil_exit\n"
      "_is_nil_true: \n\t"
      "mov rax, const3\n"
      "_is_nil_exit: \n"
      )
     'null? fvar-table const-table)))


(define code-gen-boolean?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_boolean_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_BOOL\n\t"
      "je _is_boolean_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_boolean_exit\n"
      "_is_boolean_true: \n\t"
      "mov rax, const3\n"
      "_is_boolean_exit: \n"
      )
     'boolean? fvar-table const-table)))


(define code-gen-char?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_char_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_CHAR\n\t"
      "je _is_char_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_char_exit\n"
      "_is_char_true: \n\t"
      "mov rax, const3\n"
      "_is_char_exit: \n"
      )
     'char? fvar-table const-table)))

(define code-gen-eq?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_eq_body: \n"
      
      "mov r10, qword [rbp + 4*8] \n"
      "mov r10, [r10] \n"
      "mov r11, qword [rbp + 5*8] \n"
      "mov r11, [r11] \n"
      
      "TYPE r10 \n"      
      "TYPE r11 \n"
      "cmp r11,r10 \n"
      "jne _eq_false \n"
      
      "cmp r10,T_VOID \n"
      "je L_eq_cmp_addr \n"
      
      "cmp r10, T_NIL \n"
      "je L_eq_cmp_addr \n"
      
      "cmp r10,T_BOOL \n"
      "je L_eq_cmp_addr \n"
      
      "cmp r10,T_STRING \n"
      "je L_eq_cmp_addr \n"
      
      "cmp r10,T_VECTOR \n"
      "je L_eq_cmp_addr \n"
      
      "cmp r10,T_PAIR \n"
      "je L_eq_cmp_addr \n"
      
      "cmp r10,T_CLOSURE \n"
      "je L_eq_cmp_addr \n"
      
      "cmp r10,T_INTEGER \n"
      "je L_eq_cmp_single_value \n"
      
      "cmp r10,T_SYMBOL \n"
      "je L_eq_cmp_single_value \n"
      
      "cmp r10,T_FRACTION \n"
      "je L_eq_cmp_fraction \n"
      
      "L_eq_cmp_addr: \n"
      "mov r10, qword [rbp + 4*8] \n"
      "mov r10, [r10] \n"
      "mov r11, qword [rbp + 5*8] \n"
      "mov r11, [r11] \n"
      "cmp r11,r10 \n"
      "jne _eq_false  \n"
      "jmp eq_true \n"
      
      "L_eq_cmp_fraction: \n"
      "mov r10, qword [rbp + 4*8] \n"
      "mov r10, [r10] \n"
      "mov r11, qword [rbp + 5*8] \n"
      "mov r11, [r11] \n"
      "CAR r10 \n"
      "CAR r11 \n"
      "cmp r10,r11 \n"
      "jne _eq_false \n"
      "mov r10, qword [rbp + 4*8] \n"
      "mov r10, [r10] \n"
      "mov r11, qword [rbp + 5*8] \n"
      "mov r11, [r11] \n"
      "CDR r10 \n"
      "CDR r11 \n"
      "cmp r10,r11 \n"
      "jne _eq_false  \n"
      "jmp eq_true\n"
      
      "L_eq_cmp_single_value: \n"
      "mov r10, qword [rbp + 4*8] \n"
      "mov r10, [r10] \n"
      "mov r11, qword [rbp + 5*8] \n"
      "mov r11, [r11] \n"
      "DATA r10 \n"
      "DATA r11 \n"
      "cmp r10,r11 \n"
      "jne _eq_false  \n"
      "jmp eq_true \n"
      
      "eq_true: \n"
      "mov rax,const3 \n"
      "jmp _eq_done \n"
      
      "_eq_false: \n"
      "mov rax,const2 \n"
      
      "_eq_done: \n"
      
      )'eq? fvar-table const-table)))

(define code-gen-number?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_number_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_INTEGER\n\t"
      "je _is_number_true\n\t"
      "cmp r11, T_FRACTION\n\t"
      "je _is_number_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_number_exit\n"
      "_is_number_true: \n\t"
      "mov rax, const3\n"
      "_is_number_exit: \n"
      )
     'number? fvar-table const-table)))


(define code-gen-procedure?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_procedure_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_CLOSURE\n\t"
      "je _is_procedure_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_procedure_exit\n"
      "_is_procedure_true: \n\t"
      "mov rax, const3\n"
      "_is_procedure_exit: \n"
      )
     'procedure? fvar-table const-table)))


(define code-gen-integer?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_integer_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_INTEGER\n\t"
      "je _is_integer_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_integer_exit\n"
      "_is_integer_true: \n\t"
      "mov rax, const3\n"
      "_is_integer_exit: \n"
      )
     'integer? fvar-table const-table)))



(define code-gen-rational?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_rational_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_FRACTION\n\t"
      "je _is_rational_true\n\t"
      "cmp r11, T_INTEGER\n\t"
      "je _is_rational_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_rational_exit\n"
      "_is_rational_true: \n\t"
      "mov rax, const3\n"
      "_is_rational_exit: \n"
      )
     'rational? fvar-table const-table)))


(define code-gen-zero?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_zero_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "DATA r11\n\t"
      "cmp r11, 0\n\t"
      "je _zero_true\n\t"
      "mov rax, const2\n\t"
      "jmp _zero_exit\n"
      "_zero_true: \n\t"
      "mov rax, const3\n"
      "_zero_exit: \n"
      )
     'zero? fvar-table const-table)))


(define code-gen-remainder
  (lambda(fvar-table const-table)	
    (code-gen-lambda-general
     (string-append
      "_remainder_body:\n\t"
      "mov r12, qword [rbp + 4 * 8]\n\t"
      "mov r12, [r12]\n\t"
      
      "mov r13, qword [rbp + 5 * 8]\n\t"
      "mov r13, [r13]\n\t"

      "cmp r12, 0\n\t"
      "jl _remainder_negative\n"
 
      "_remainder_positive:\n\t"
      "DATA r12\n\t"
      "DATA r13\n\t"
      "xor rdx, rdx\n\t"
      "mov rax, r12\n\t"
      "idiv r13\n\t"
      "mov r12, rax\n\t"
      "mov r12, rdx\n\t"
      "shl r12, TYPE_BITS\n\t"
      "add r12, T_INTEGER\n\t"
      "jmp _remainder_finish\n"
      
      "_remainder_negative:\n\t"
      "DATA r12\n\t"
      "neg r12\n\t"
      "DATA r13\n\t"
      "xor rdx, rdx\n\t"
      "mov rax, r12\n\t"
      "idiv r13\n\t"
      "mov r12, rax\n\t"
      "neg rdx\n\t"
      "mov r12, rdx\n\t"
      "shl r12, TYPE_BITS\n\t"
      "add r12, T_INTEGER\n\t"
      "jmp _remainder_finish\n"
      
      "_remainder_finish:\n"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov [rax], r12\n"
      )
     'remainder fvar-table const-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;symbol

(define code-gen-symbol?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_symbol_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_SYMBOL\n\t"
      "je _symbol_true\n\t"
      "mov rax, const2\n\t"
      "jmp _symbol_exit\n"
      "_symbol_true: \n\t"
      "mov rax, const3\n"
      "_symbol_exit: \n"
      )
     'symbol? fvar-table const-table)))





(define code-gen-symbol->string
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_symbol_to_string_body: \n\t"
      "mov r11, [rbp + 4*8]\n\t"
      "mov r11, [r11]\n\t"
      "SYMBOL_NAME rax, r11\n\t"
      "_symbol_to_string_finish:\n"
      )
     'symbol->string fvar-table const-table)))


(define code-gen-string->symbol
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_string_to_symbol_body: \n\t"
      "mov r11, [rbp + 4*8]\n\t"
      "mov r10, [symbol_start]\n\t"
      "mov r10, [r10]\n\t"
      "cmp r10, [const1]\n\t"
      
      "je _string_to_symbol_create \n\t"
      "_string_to_symbol_loop: \n\t"
      "mov r12, r10\n\t"          ;backup

      "CAR r12\n\t"
      "SYMBOL_NAME r12, r12\n\t"
      "EQUAL_STRINGS r11, r12\n\t"
      "cmp rax, [const3]\n\t"
      
      "je _string_to_symbol_found \n\t"
      
      "CDR_POINTR r10\n\t"
      "mov r10, [r10]\n\t"
      "cmp r10, [const1]\n\t"
      "je _string_to_symbol_create\n\t"
      "jmp _string_to_symbol_loop\n\t"
      
      
      "_string_to_symbol_found:\n"
      "CAR_POINTR r10\n"
      "mov rax, r10\n"
      "jmp _string_to_symbol_finish\n\t"
      
      "_string_to_symbol_create: \n\t"
      "mov rdi,8\n\t"
      "call malloc\n\t"
      "MAKE_LITERAL_SYMBOL2 rax , r11\n"
      "mov r11, rax\n"
      "mov r13, r11\n"                    ;backup
      "mov r14, [symbol_start]\n\t"       
      
      "MAKE_LITERAL_PAIR2 r11 ,r14\n\n"
      
       "mov rdi, 8\n\t"
       "call malloc\n\t"
      "mov [rax], r11\n"
      "mov [symbol_start],rax\n"
      "mov rax, r13\n\n"
      
      "_string_to_symbol_finish:\n"
      
      )
     'string->symbol fvar-table const-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;list


(define code-gen-car
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_car_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "CAR r11\n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov qword[rax], r11\n\t"
      "_car_finish:\n"
      )
     'car fvar-table const-table)))

(define code-gen-cdr
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_cdr_body: \n\t" 
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "CDR r11\n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov qword[rax], r11\n\t"
      "_cdr_finish:\n"
      )
     'cdr fvar-table const-table)))

(define code-gen-cons
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_cons_body: \n\t"
      
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "mov r12, qword[rbp+5*8]\n\t"
      "mov r12, qword[r12]\n\t"
      
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov r13, rax\n\t"
      "mov qword[r13], r11\n\t"
      
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov r14, rax\n\t"
      "mov qword[r14], r12\n\t"
      
      "mov rdi, 16\n\t"
      "call malloc\n\t"
      "MAKE_MALLOC_LITERAL_PAIR rax, r13, r14\n"
      )
     'cons fvar-table const-table)))



(define code-gen-pair?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_pair_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_PAIR\n\t"
      "je _is_pair_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_pair_exit\n"
      "_is_pair_true: \n\t"
      "mov rax, const3\n"
      "_is_pair_exit: \n"
      )
     'pair? fvar-table const-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;vector


(define code-gen-vector?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_vector_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_VECTOR\n\t"
      "je _is_vector_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_vector_exit\n"
      "_is_vector_true: \n\t"
      "mov rax, const3\n"
      "_is_vector_exit: \n"
      )
     'vector? fvar-table const-table)))

     
     
(define code-gen-vector-length
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_vector_length_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "VECTOR_LENGTH r11\n\t"
      "shl r11,4\n\t"
      "add r11,T_INTEGER\n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov qword[rax], r11\n"
      )
     'vector-length fvar-table const-table)))



(define code-gen-vector-ref
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_vector_ref_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "mov r12, qword[rbp+5*8]\n\t"
      "mov r12, qword[r12]\n\t"
      "DATA r12\n\t"
      "VECTOR_REF rbx, r11, r12\n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov qword[rax], rbx\n\t"
      "_vector_ref_finish:\n"
      )
     'vector-ref fvar-table const-table)))



(define code-gen-vector-set!
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_vector_set_body :\n\t"
      "mov r11, qword[rbp+4*8]\n\t" ;vec
      "mov r11, qword[r11]\n"
      "mov r12, qword[rbp+5*8]\n\t" ;pos
      "mov r12, qword[r12]\n\t"
      "DATA r12\n\t"
      "mov r13, qword[rbp+6*8]\n\t" ;value
   ;   "mov r13,qword[r13]\n\t"
   ;   "DATA r13\n\t"
   ;   "shl r12, 3\n\t"
    ;  "add r11 , r12\n\t"
      "VECTOR_ELEMENTS r11\n\t"
      "mov [r11 + r12*8], r13\n"
      "mov rax, const0\n\t"
      "_vector_set_finish:\n"
      )
     'vector-set! fvar-table const-table)))


(define code-gen-vector
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append    
      "custom_vector_body:\n"       
      "mov rbx, arg_count\n"
      
      "push rbx\n"
      "shl rbx, 3\n"
      "mov rdi, rbx\n"
      "call malloc\n"
      "pop rbx\n"
      
      ;rax= pointer to address of rbx*8 bytes, rbx=length of vector
      "mov r10, 0\n" ;counter
      "for_vector:\n"
      "cmp r10, rbx\n"
      "je end_of_vector\n"
      
      "mov rdx, An(r10)\n" 
      "mov qword [rax+r10*8], rdx\n"
      "inc r10\n"
      "jmp for_vector\n"
      "end_of_vector:\n"
      
      "mov rcx, rax\n"
      "shl rbx, 3\n"
      "MAKE_LITERAL_VECTOR2 rcx, rbx\n" ;at the end of macro, rax is a literal vector
      "push rcx\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "pop rcx\n"
      "mov [rax], rcx\n"
      
      "custom_vector_finish:\n")
     'vector fvar-table const-table)))
  
 

(define code-gen-make-vector
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append    
      "make_vector_body:\n"        
      "mov rbx, arg_count\n"
      "cmp rbx, 1\n" 
      "je make_vector_null\n"
      
      "make_vector_val:\n\t"
      "mov rax, [rbp+4*8]\n" ;length of vector	        
      "mov rax, [rax]\n"
      "mov rbx, rax\n"
      "DATA rbx\n" 
      
      "mov rdx, [rbp+5*8]\n" ; address of item
  
      "push rbx\n"
      "push rdx\n"
      "shl rbx, 3\n"
      "mov rdi, rbx\n"
      "call malloc\n"
      "pop rdx\n"
      "pop rbx\n"

      "mov r10, 0\n" 
      
      "for_create_vector:\n"
      "cmp r10, rbx\n"
      "je end_of_create_vector\n"
      "mov qword [rax+r10*8], rdx\n"
      "inc r10\n"
      "jmp for_create_vector\n"
      "end_of_create_vector:\n"
      
      "mov rcx, rax\n"
      "shl rbx, 3\n"
      "MAKE_LITERAL_VECTOR2 rcx, rbx\n"
      "push rcx\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "pop rcx\n"
      "mov [rax], rcx\n"
      "jmp make_vector_finish\n"
      
      
      "make_vector_null:\n\t"
      "mov rax, [rbp+4*8]\n" ;length of vector	        
      "mov rax, [rax]\n"
      "mov rbx, rax\n"
      "DATA rbx\n" 
      
      "push rbx\n"
      "shl rbx, 3\n"
      "mov rdi, rbx\n"
      "call malloc\n"
      "mov r11, rax\n\t"
      "pop rbx\n"
      "mov rdx,0\n\t"
      "shl rdx, TYPE_BITS\n\t"
      "add rdx, T_INTEGER\n\t" 
      "mov r10, 0\n" 
      
      "mov rdi,8\n\t"
      "push rdx\n\t"
      "call malloc\n\t"
      "pop rdx\n\t"
      "mov [rax], rdx\n\t"
      

      "for_create_vector_nul:\n"
      "cmp r10, rbx\n"
      "je end_of_create_vector_nul\n"
      
      "mov qword [r11+r10*8], rax\n"
      "inc r10\n"
      "jmp for_create_vector_nul\n"
      "end_of_create_vector_nul:\n"
      
      "mov rcx, r11\n"
      "shl rbx, 3\n"
      "MAKE_LITERAL_VECTOR2 rcx, rbx\n"
      "push rcx\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "pop rcx\n"
      "mov [rax], rcx\n"
      
 
      "make_vector_finish:\n"
      )
     'make-vector fvar-table const-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;string

(define code-gen-string?
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_is_string_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "TYPE r11\n\t"
      "cmp r11, T_STRING\n\t"
      "je _is_string_true\n\t"
      "mov rax, const2\n\t"
      "jmp _is_string_exit\n"
      "_is_string_true: \n\t"
      "mov rax, const3\n"
      "_is_string_exit: \n"
      )
     'string? fvar-table const-table)))



(define code-gen-string-length
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_string_length_body :\n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "STRING_LENGTH r11\n\t"
      "shl r11,4\n\t"
      "add r11,T_INTEGER\n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov qword[rax], r11\n"
      )
     'string-length fvar-table const-table)))




(define code-gen-string-ref
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_string_ref_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t" ;string
      "mov r11, qword[r11]\n\t"
      "mov r12, qword[rbp+5*8]\n\t" ;pos
      "mov r12, qword[r12]\n\t"
      "DATA r12\n\t"
      "mov r13, 0\n\t"
      "STRING_REF r13B, r11, r12\n\t"
      "sal r13, 4\n"
      "or r13, T_CHAR\n"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov qword[rax], r13\n\t"
      "_string_ref_finish:\n"
      )
     'string-ref fvar-table const-table)))




(define code-gen-string-set!
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append      
      "string_set_body:\n\t"
      
      "mov r11, qword[rbp+4*8]\n\t" ;string
      "mov r11, qword[r11]\n\t"
      
      "mov r12, qword[rbp+5*8]\n\t" ;pos
      "mov r12, qword[r12]\n\t"
      "DATA r12\n\t"
      
      "mov r13, An(2)\n\t" ;char
      "mov r13, [r13]\n\t"      
      "DATA r13\n\t"

      ;rbx=string, rdx=index, rcx=char
      
      "STRING_ELEMENTS r11\n\t"
      "add r11, r12\n\t"
      "mov rcx, r13\n\t"
      "mov byte [r11], cl\n\t"
      
      "mov rax, const0\n"
      
      "string_set_finish:\n"
      
      )
     'string-set! fvar-table const-table)))



(define code-gen-make-string
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append          
      "make_string_body:\n"
      "mov rdx, qword 0\n" ;initialize char with 0         
      "mov r9, arg_count\n"
      "cmp r9, 2\n"
      "jg make_string_finish\n"
      
      "mov rax, An(0)\n" ;length of string	        
      "mov rax, [rax]\n"
      "mov rbx, rax\n"
      "DATA rbx\n" 
      
      "TYPE rax\n"
      "cmp rax, T_INTEGER\n"
      "jne make_string_finish\n"
      
      "cmp r9, 1\n"
      "je start_creating_string\n"
      
      "mov rcx, An(1)\n" ;char
      "mov rcx, [rcx]\n"
      "mov rdx, rcx\n"
      "DATA rdx\n"
      
      "TYPE rcx\n"
      "cmp rcx, T_CHAR\n"
      "jne make_string_finish\n"
      
      "start_creating_string:\n"
      
      "push rbx\n"
      "push rdx\n"
      "mov rdi, rbx\n"
      "call malloc\n"
      "pop rdx\n"
      "pop rbx\n"
      
      
      ;rax= pointer to address of rbx bytes, rbx=length of string, rdx=char
      "mov r10, 0\n" ;counter
      
      "for_create_string:\n"
      "cmp r10, rbx\n"
      "je end_of_create_string\n"
      "mov byte [rax+r10], dl\n"
      "inc r10\n"
      "jmp for_create_string\n"
      "end_of_create_string:\n"
      
      "mov rcx, rax\n"
      
      "MAKE_LITERAL_STRING2 rcx, rbx\n" ;at the end of macro, rax is a literal string

      "push rcx\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "pop rcx\n"
      
      "mov [rax], rcx\n"
      
      "make_string_finish:\n"
      )
     'make-string fvar-table const-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;fractions


(define code-gen-denominator
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_denominator_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "mov r12, r11\n\t"
      "TYPE r12\n\t"
      "cmp r12, T_INTEGER\n\t"
      "je denominator_is_integer\n\t"
      "CDR r11\n\t"
      "jmp denominator_exit\n"
      "denominator_is_integer: \n\t"
      "mov r11, 1\n"
      "shl r11, TYPE_BITS\n\t"
      "add r11, T_INTEGER\n\t"
      "denominator_exit: \n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov qword[rax], r11\n"
      )
     'denominator fvar-table const-table)))




(define code-gen-numerator
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append
      "_numerator_body: \n\t"
      "mov r11, qword[rbp+4*8]\n\t"
      "mov r11, qword[r11]\n\t"
      "mov r12, r11\n\t"
      "TYPE r12\n\t"
      "cmp r12, T_INTEGER\n\t"
      "je numerator_exit\n\t"
      "CAR r11\n\t"
      "jmp numerator_exit\n"
      "numerator_exit: \n\t"
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov qword[rax], r11\n"              
      )
     'numerator fvar-table const-table)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;plus


(define code-gen-plus
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append 
      "_plus_body:\n\t"
      "mov rdi, 0\n\t"                 ;loop index           
      "mov r10, 0\n\t"			;init numenator
      "mov r11, 1\n"			;init denumintor
      
      "_plus_loop_label: \n\t"
      "cmp rdi, qword[rbp + 3*8]\n\t"
      "je _plus_end_loop\n\t"
      
      "mov r12, qword[rbp + (4 +rdi)*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _plus_is_integer\n\t"
      
      "_plus_is_fraction:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _plus_add_fraction_to_accs\n"
      
      "_plus_is_integer:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      "_plus_add_fraction_to_accs: \n\t"
      
      "mov rax, r15\n\t"
      "mul r10\n\t"					;r10 contains common right numinator
      "mov r10, rax\n\t"
      
      "mov rax, r11\n\t"
      "mul r14\n\t"					;r14 contains common left numinator
      "mov r14, rax\n\t"
      
      "mov rax, r15\n\t"
      "mul r11\n\t"					;r11 contains common denominator
      "mov r11, rax\n\t"
      
      "add r10, r14\n\t"                               ;r10 contain numinator, r11 contain denominator
      
      "inc rdi\n\t"
      "jmp _plus_loop_label\n"
      
      "_plus_end_loop:\n\t"	   
      "cmp r11,1\n\t"
      "je _plus_final_is_int\n\t"
      
      "NUM_GCD r10,r11\n\t"			;result in rdi
      "mov rbx, rdi\n\t"
      
      "mov rax, r11\n\t"
      "mov rdx, 0\n"
      "CQO\n"
      "idiv rbx\n\t"
      "mov r11, rax\n\t"
      
      "mov rax, r10\n\t"
      "mov rdx, 0\n"
      "CQO\n"
      "idiv rbx\n\t"
      "mov r10, rax\n\t"
      
      
      "cmp r11, 0\n\t"
      "jge _plus_make_literal\n\t"
      "neg r11\n\t"
      "neg r10\n\t"
       
      
       "_plus_make_literal:\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "mov r12, rax\n\t"
       "shl r10, TYPE_BITS\n\t"
       "add r10, T_INTEGER\n\t"
       "mov [r12], r10\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "mov r13, rax\n\t"
       "shl r11, TYPE_BITS\n\t"
       "add r11, T_INTEGER\n\t"
       "mov [r13], r11\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       
       "MAKE_MALLOC_LITERAL_FRACTION rax, r12, r13\n\t"
       
       "jmp _plus_exit\n\t"
       
       "_plus_final_is_int:\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "shl r10, TYPE_BITS\n\t"
       "add r10, T_INTEGER\n\t"
       "mov [rax], r10\n"
       
       "_plus_exit:\n"
       
       )
     '+ fvar-table const-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;multiply


(define code-gen-multi
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append 
      "_multi_body:\n\t"
      "mov rdi, 0\n\t"                 ;loop index           
      "mov r10, 1\n\t"			;init numenator
      "mov r11, 1\n"			;init denumintor
      
      "_multi_loop_label: \n\t"
      "cmp rdi, qword[rbp + 3*8]\n\t"
      "je _multi_end_loop\n\t"
      
      "mov r12, qword[rbp + (4 +rdi)*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _multi_is_integer\n\t"
      
      "_multi_is_fraction:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _multi_mul_fraction_to_accs\n"
      
      "_multi_is_integer:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      "_multi_mul_fraction_to_accs: \n\t"
      
      "mov rax, r14\n\t"
      "mul r10\n\t"					;r10 contains new numinator
      "mov r10, rax\n\t"
      
      "mov rax, r15\n\t"
      "mul r11\n\t"					;r14 contains new denominator
      "mov r11, rax\n\t"
      
      
      "inc rdi\n\t"
      "jmp _multi_loop_label\n"
      
      "_multi_end_loop:\n\t"	   
      "cmp r11,1\n\t"
      "je _multi_final_is_int\n\t"
      
      "NUM_GCD r10,r11\n\t"			;result in rdi
      
      "mov rbx, rdi\n\t"
      
      "mov rax, r11\n\t"
      "mov rdx, 0\n"
      "CQO\n"
      "idiv rbx\n\t"
      "mov r11, rax\n\t"
      
      "mov rax, r10\n\t"
      "mov rdx, 0\n"
      "CQO\n"
      "idiv rbx\n\t"
      "mov r10, rax\n\t"
      
      
      "cmp r11, 0\n\t"
      "jge _mul_make_literal\n\t"
      "neg r11\n\t"
      "neg r10\n\t"
      
      
      "_mul_make_literal:\n\t"
      
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov r12, rax\n\t"
      "shl r10, TYPE_BITS\n\t"
      "add r10, T_INTEGER\n\t"
      "mov [r12], r10\n\t"

      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "mov r13, rax\n\t"
      "shl r11, TYPE_BITS\n\t"
      "add r11, T_INTEGER\n\t"
      "mov [r13], r11\n\t"
      
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      
      "MAKE_MALLOC_LITERAL_FRACTION rax, r12, r13\n\t"
      
      "jmp _multi_exit\n\t"
      
      "_multi_final_is_int:\n\t"
      
      "mov rdi, 8\n\t"
      "call malloc\n\t"
      "shl r10, TYPE_BITS\n\t"
      "add r10, T_INTEGER\n\t"
      "mov [rax], r10\n"
      
      "_multi_exit:\n"
      
      )
     '* fvar-table const-table)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;div


(define code-gen-div
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append 
      "_div_body:\n\t"
      
      "mov r12, qword[rbp + 4*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _div_is_integer\n\t"
      
      "_div_is_fraction:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _div_begin_check\n"
      
      "_div_is_integer:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      "_div_begin_check:\n\t"
      "mov rdi, qword[rbp + 3*8]\n\t"
      "cmp rdi, 1\n\t"
      "je _div_with_one_arg\n"
      
      ;loop
      
      "mov rdi, 1\n\t"
      "mov r10, r14\n\t"
      "mov r11, r15\n\t"
      "_div_loop_label: \n\t"
      "cmp rdi, qword[rbp + 3*8]\n\t"
      "je _div_end_loop\n\t"
      
      "mov r12, qword[rbp + (4 +rdi)*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _div_is_integer2\n\t"
      
      "_div_is_fraction2:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _div_div_fraction_to_accs\n"
      
      "_div_is_integer2:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      "_div_div_fraction_to_accs: \n\t"
      
      "mov rax, r15\n\t"
      "mul r10\n\t"					;r10 contains new numinator
      "mov r10, rax\n\t"
      
      "mov rax, r14\n\t"
      "mul r11\n\t"					;r14 contains new denominator
      "mov r11, rax\n\t"
      
      "inc rdi\n\t"
      "jmp _div_loop_label\n"
      

      ;end loop
      
       "_div_with_one_arg:\n\t"
       "mov r10, 1\n\t"
       "mov r11, 1\n\t"
       
       "mov rax, r15\n\t"
       "mul r10\n\t"					;r10 contains new numinator
       "mov r10, rax\n\t"
       
       "mov rax, r14\n\t"
       "mul r11\n\t"					;r14 contains new denominator
       "mov r11, rax\n\t"
       
       ;end loop for div with 2 arguments
       
       "_div_end_loop:\n\t"	   
       "cmp r11,1\n\t"
       "je _div_final_is_int\n\t"
       
       "NUM_GCD r10,r11\n\t"			;result in rdi
       
       "mov rbx, rdi\n\t"
       
       "mov rax, r11\n\t"
       "mov rdx, 0\n"
       "CQO\n"
       "idiv rbx\n\t"
       "mov r11, rax\n\t"
       
       "mov rax, r10\n\t"
       "mov rdx, 0\n"
       "CQO\n"
       "idiv rbx\n\t"
       "mov r10, rax\n\t"
       
       
       "cmp r11, 0\n\t"
       "jge _div_make_literal\n\t"
       "neg r11\n\t"
       "neg r10\n\t"
       

       "_div_make_literal:\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "mov r12, rax\n\t"
       "shl r10, TYPE_BITS\n\t"
       "add r10, T_INTEGER\n\t"
       "mov [r12], r10\n\t"

       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "mov r13, rax\n\t"
       "shl r11, TYPE_BITS\n\t"
       "add r11, T_INTEGER\n\t"
       "mov [r13], r11\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       
       "MAKE_MALLOC_LITERAL_FRACTION rax, r12, r13\n\t"

       "jmp _div_exit\n\t"
       
       "_div_final_is_int:\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "shl r10, TYPE_BITS\n\t"
       "add r10, T_INTEGER\n\t"
       "mov [rax], r10\n"
       
       "_div_exit:\n"
       
       )
     '/ fvar-table const-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;minus


(define code-gen-minus
  (lambda (fvar-table const-table)
      (code-gen-lambda-general
       (string-append 
       "_minus_body:\n\t"

       "mov r12, qword[rbp + 4*8]\n\t"	
       "mov r12, [r12]\n\t"                  ;current arg in r12
       "mov r13, r12\n\t"                    ;typecheck on r13
       "TYPE r13\n\t"
       "mov r14, r12\n\t"
       "mov r15, r12\n\t"
       "cmp r13, T_INTEGER\n\t"
       "je _minus_is_integer\n\t"

       "_minus_is_fraction:\n\t"                      
       "CAR r14\n\t"                         ;numerator in r14 
       "DATA r14\n\t"
       "CDR r15\n\t"                         ;denominator in r15
       "DATA r15\n\t"
       "jmp _minus_begin_check\n"

       "_minus_is_integer:\n\t"
       "DATA r14\n\t"
       "mov r15, 1\n"                        ;denominator in r15 is 1
       
       "_minus_begin_check:\n\t"
       "mov rdi, qword[rbp + 3*8]\n\t"
       "cmp rdi, 1\n\t"
       "je _minus_with_one_arg\n"

;loop
       
       "mov rdi, 1\n\t"
       "mov r10, r14\n\t"
       "mov r11, r15\n\t"
       "_minus_loop_label: \n\t"
       "cmp rdi, qword[rbp + 3*8]\n\t"
       "je _minus_end_loop\n\t"
       
       "mov r12, qword[rbp + (4 +rdi)*8]\n\t"	
       "mov r12, [r12]\n\t"                  ;current arg in r12
       "mov r13, r12\n\t"                    ;typecheck on r13
       "TYPE r13\n\t"
       "mov r14, r12\n\t"
       "mov r15, r12\n\t"
       "cmp r13, T_INTEGER\n\t"
       "je _minus_is_integer2\n\t"

       "_minus_is_fraction2:\n\t"                      
       "CAR r14\n\t"                         ;numerator in r14 
       "DATA r14\n\t"
       "CDR r15\n\t"                         ;denominator in r15
       "DATA r15\n\t"
       "jmp _minus_minus_fraction_to_accs\n"

       "_minus_is_integer2:\n\t"
       "DATA r14\n\t"
       "mov r15, 1\n"                        ;denominator in r15 is 1
      
       "_minus_minus_fraction_to_accs: \n\t"
       
       "mov rax, r15\n\t"
       "mul r10\n\t"					;r10 contains common right numinator
       "mov r10, rax\n\t"
       
       "mov rax, r11\n\t"
       "mul r14\n\t"					;r14 contains common left numinator
       "mov r14, rax\n\t"
       
       "mov rax, r15\n\t"
       "mul r11\n\t"					;r11 contains common denominator
       "mov r11, rax\n\t"
       
       "sub r10, r14\n\t"                               ;r10 contain numinator, r11 contain denominator
       
       "inc rdi\n\t"
       "jmp _minus_loop_label\n"


;end loop

       "_minus_with_one_arg:\n\t"
       "mov r10, 0\n\t"
       "mov r11, 1\n\t"
       
       "mov rax, r15\n\t"
       "mul r10\n\t"					;r10 contains common right numinator
       "mov r10, rax\n\t"
       
       "mov rax, r11\n\t"
       "mul r14\n\t"					;r14 contains common left numinator
       "mov r14, rax\n\t"
       
       "mov rax, r15\n\t"
       "mul r11\n\t"					;r11 contains common denominator
       "mov r11, rax\n\t"
       
       "sub r10, r14\n\t"                               ;r10 contain numinator, r11 contain denominator

       ;end loop for minus with 2 arguments
       
       "_minus_end_loop:\n\t"	   
       "cmp r11,1\n\t"
       "je _minus_final_is_int\n\t"
       
       "NUM_GCD r10,r11\n\t"			;result in rdi
       
       "mov rbx, rdi\n\t"
       
       "mov rax, r11\n\t"
       "mov rdx, 0\n"
       "CQO\n"
       "idiv rbx\n\t"
       "mov r11, rax\n\t"
       
       "mov rax, r10\n\t"
       "mov rdx, 0\n"
       "CQO\n"
       "idiv rbx\n\t"
       "mov r10, rax\n\t"


       "cmp r11, 0\n\t"
       "jge _minus_make_literal\n\t"
       "neg r11\n\t"
       "neg r10\n\t"
       

       "_minus_make_literal:\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "mov r12, rax\n\t"
       "shl r10, TYPE_BITS\n\t"
       "add r10, T_INTEGER\n\t"
       "mov [r12], r10\n\t"

       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "mov r13, rax\n\t"
       "shl r11, TYPE_BITS\n\t"
       "add r11, T_INTEGER\n\t"
       "mov [r13], r11\n\t"

       "mov rdi, 8\n\t"
       "call malloc\n\t"
   
       "MAKE_MALLOC_LITERAL_FRACTION rax, r12, r13\n\t"

       "jmp _minus_exit\n\t"
       
       "_minus_final_is_int:\n\t"
       
       "mov rdi, 8\n\t"
       "call malloc\n\t"
       "shl r10, TYPE_BITS\n\t"
       "add r10, T_INTEGER\n\t"
       "mov [rax], r10\n"
       
       "_minus_exit:\n"

       )
       '- fvar-table const-table)))






(define code-gen-<
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append 
      "_lower_then_body:\n\t"
      "mov rsi, qword [rbp + 3*8]\n\t";       
      "cmp rsi, 1\n\t"
      "je _lower_then_true \n\t"
      
      "mov r12, qword[rbp + 4*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _lower_then_is_integer\n\t"
      
      "_lower_then_is_fraction:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _lower_then_start_loop\n"
      
      "_lower_then_is_integer:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      
      ;loop
      
      "_lower_then_start_loop:\n\t"
      "mov rdi, 1\n\t"
      "mov r10, r14\n\t"
      "mov r11, r15\n\t"
      "_lower_then_loop_label: \n\t"
      "cmp rdi, qword[rbp + 3*8]\n\t"
      "je _lower_then_true\n\t"
      
      "mov r12, qword[rbp + (4 +rdi)*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _lower_then_is_integer2\n\t"
      
      "_lower_then_is_fraction2:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _lower_then_fraction_to_accs\n"
      
      "_lower_then_is_integer2:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      "_lower_then_fraction_to_accs: \n\t"
      
      
      
      "mov r8, r14\n\t"				
      "mov r9, r15\n\t"			
      
      "mov rax, r15\n\t"
      "mul r10\n\t"					;r10 contains common right numinator
      "mov r10, rax\n\t"
      
      "mov rax, r11\n\t"
      "mul r14\n\t"					;r14 contains common left numinator
      "mov r14, rax\n\t"
      
      "cmp r10, r14\n\t"
      "jge _lower_then_false \n\t"
      
      "mov r10, r8\n\t"
      "mov r11, r9\n\t"
      
      
      "inc rdi\n\t"
      "jmp _lower_then_loop_label\n"
      
      
      ;end loop
      
      
      
      
      "_lower_then_false:\n\t"
      "mov rax, const2\n\t"
      "jmp _lower_then_exit\n"
      
      "_lower_then_true:\n\t"
      "mov rax, const3\n\t"
      
      "_lower_then_exit:\n"
      
      )
     '< fvar-table const-table)))






(define code-gen->
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append 
      "_greater_then_body:\n\t"
      "mov rsi, qword [rbp + 3*8]\n\t";       
      "cmp rsi, 1\n\t"
      "je _greater_then_true \n\t"
      
      "mov r12, qword[rbp + 4*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _greater_then_is_integer\n\t"
      
      "_greater_then_is_fraction:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _greater_then_start_loop\n"
      
      "_greater_then_is_integer:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      
      ;loop
      
      "_greater_then_start_loop:\n\t"
      "mov rdi, 1\n\t"
      "mov r10, r14\n\t"
      "mov r11, r15\n\t"
      "_greater_then_loop_label: \n\t"
      "cmp rdi, qword[rbp + 3*8]\n\t"
      "je _greater_then_true\n\t"
      
      "mov r12, qword[rbp + (4 +rdi)*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _greater_then_is_integer2\n\t"
      
      "_greater_then_is_fraction2:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _greater_then_fraction_to_accs\n"
      
      "_greater_then_is_integer2:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      "_greater_then_fraction_to_accs: \n\t"
      
      
      
      "mov r8, r14\n\t"				
      "mov r9, r15\n\t"			
      
      "mov rax, r15\n\t"
      "mul r10\n\t"					;r10 contains common right numinator
      "mov r10, rax\n\t"
      
      "mov rax, r11\n\t"
      "mul r14\n\t"					;r14 contains common left numinator
      "mov r14, rax\n\t"
      
      "cmp r10, r14\n\t"
      "jle _greater_then_false \n\t"
      
      "mov r10, r8\n\t"
      "mov r11, r9\n\t"
      
      
      "inc rdi\n\t"
      "jmp _greater_then_loop_label\n"
      
      
      ;end loop
      
      
      
      
      "_greater_then_false:\n\t"
      "mov rax, const2\n\t"
      "jmp _greater_then_exit\n"
      
      "_greater_then_true:\n\t"
      "mov rax, const3\n\t"
      
      "_greater_then_exit:\n"
      
      )
     '> fvar-table const-table)))





(define code-gen-=
  (lambda (fvar-table const-table)
    (code-gen-lambda-general
     (string-append 
      "_equal_body:\n\t"
      "mov rsi, qword [rbp + 3*8]\n\t";       
      "cmp rsi, 1\n\t"
      "je _equal_true \n\t"
      
      "mov r12, qword[rbp + 4*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _equal_is_integer\n\t"
      
      "_equal_is_fraction:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _equal_start_loop\n"
      
      "_equal_is_integer:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      
      ;loop
      
      "_equal_start_loop:\n\t"
      "mov rdi, 1\n\t"
      "mov r10, r14\n\t"
      "mov r11, r15\n\t"
      "_equal_loop_label: \n\t"
      "cmp rdi, qword[rbp + 3*8]\n\t"
      "je _equal_true\n\t"
      
      "mov r12, qword[rbp + (4 +rdi)*8]\n\t"	
      "mov r12, [r12]\n\t"                  ;current arg in r12
      "mov r13, r12\n\t"                    ;typecheck on r13
      "TYPE r13\n\t"
      "mov r14, r12\n\t"
      "mov r15, r12\n\t"
      "cmp r13, T_INTEGER\n\t"
      "je _equal_is_integer2\n\t"
      
      "_equal_is_fraction2:\n\t"                      
      "CAR r14\n\t"                         ;numerator in r14 
      "DATA r14\n\t"
      "CDR r15\n\t"                         ;denominator in r15
      "DATA r15\n\t"
      "jmp _equal_fraction_to_accs\n"
      
      "_equal_is_integer2:\n\t"
      "DATA r14\n\t"
      "mov r15, 1\n"                        ;denominator in r15 is 1
      
      "_equal_fraction_to_accs: \n\t"
      
      
      
      "mov r8, r14\n\t"				
      "mov r9, r15\n\t"			
      
      "mov rax, r15\n\t"
      "mul r10\n\t"					;r10 contains common right numinator
      "mov r10, rax\n\t"
      
      "mov rax, r11\n\t"
      "mul r14\n\t"					;r14 contains common left numinator
      "mov r14, rax\n\t"
      
      "cmp r10, r14\n\t"
      "jne _equal_false \n\t"
      
      "mov r10, r8\n\t"
      "mov r11, r9\n\t"
      
      
      "inc rdi\n\t"
      "jmp _equal_loop_label\n"
      
      
      ;end loop
      
      
      
      
      "_equal_false:\n\t"
      "mov rax, const2\n\t"
      "jmp _equal_exit\n"
      
      "_equal_true:\n\t"
      "mov rax, const3\n\t"
      
      "_equal_exit:\n"
      
      )
     '= fvar-table const-table)))







(define make-label
  (lambda (prefix)
    (lambda ()
      (let ((n 0))
        (lambda ()
          (set! n (+ n 1))
          (string-append
           prefix (number->string n)))))))


(define ifexit_label ((make-label "L_if3_exit_")))
(define ifelse_label ((make-label "L_if3_else_")))
(define for_label ((make-label "L_for_")))
(define forexit_label ((make-label "L_for_exit_")))
(define or_exit_label ((make-label "L_or_exit_")))
(define clos_body_label ((make-label "L_clos_body_")))
(define clos_exit_label ((make-label "L_exit_body_")))
(define print_label ((make-label "DONT_PRINT_")))
(define fix_stack_label ((make-label "FIX_STACK_")))
(define substraction_label ((make-label "L_substraction_")))
(define empty_list_params_label ((make-label "_empty_list_params")))
(define continue_label ((make-label "L_continue_")))



(define list-of-strings->string
	(lambda (lst)
		(if (null? lst) ""
			(string-append (car lst) (list-of-strings->string (cdr lst))))))








(compile-scheme-file "src.scm" "src.s")
