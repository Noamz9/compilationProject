#lang racket
(define code-gen-integer->char
  (lambda (exp const-table major)
    (string-append
     "integer_to_char: \n"
     "push rbp\n"
     "mov rbp, rsp \n"
     "mov rax, qword[rbp+3*8]\n"
     "xor rax, (T_CHAR ^ T_INTEGER) \n"  ; ^ is bitwise exclusive or
     "leave\n"
     "ret\n"   
     )))

(define code-gen-char->integer
  (lambda (exp const-table major)
    (string-append
     "char_to_integer: \n"
     "push rbp\n"
     "mov rbp, rsp \n"
     "mov rax, qword[rbp+3*8]\n"
     "xor rax, (T_CHAR ^ T_INTEGER) \n"
     "leave\n"
     "ret\n"
   
     )))

(define code-gen-car
	(lambda (fvar-table)
			(string-append
				"push rbp;\n"
  				"mov rbp, rsp;\n"
  			;	"mov rax, qword[rbp+4*8]\n"
                        ;       "cmp rax, 1\n"
 			;	"jne e_bad_args_error\n"
 				"mov rax, qword[rbp+3*8]\n" ;; A0
                        ;       "mov rbx , rax\n"
  			;	"Type rbx\n"
			;	"cmp rbx , T_PAIR\n"
			;	"jne e_bad_type\n"
                                "CAR rax"
                                "leave\n"
                                "ret\n"
				) 'car fvar-table
		))




(define code-gen-cdr
	(lambda (fvar-table)
			(string-append
				"push rbp;\n"
  				"mov rbp, rsp;\n"
  				;"mov rax, qword[rbp+4*8]\n"
                                ;"cmp rax, 1\n"
 				;"jne e_bad_args_error\n"
 				"mov rax, qword[rbp+3*8]\n"
                                ;"mov rbx , rax\n"
  				;"Type rbx\n"
				;"cmp rbx , T_PAIR\n"
				;"jne e_bad_type\n"
                                "CDR rax\n"
                                "leave\n"
                                "ret\n"
				) 'cdr fvar-table
		))
6 

(define code-gen-cons
	(lambda (fvar-table)
			(string-append
				"CONS:"
				"push rbp;\n"
  				"mov rbp, rsp;\n"
  				;"mov rax, qword[rbp+4*8]\n"
                                ;"cmp rax, 2\n"
 				;"jne e_bad_args_error\n"
 				"mov rax A0\n"
 				"mov rbx A1\n"      
                                "MAKE_LITERAL_PAIR rax, rbx\n"
				"leave\n"
				"ret\n"
				) 'cons fvar-table
		))


(define code-gen-integer?
	(lambda (fvar-table)
			(string-append
				"push rbp;\n"
  				"mov rbp, rsp;\n"
    				;"mov rax, qword[rbp+4*8]\n"
                                ;"cmp rax, 2\n"
 				;"jne e_bad_args_error\n"
 				"mov rax, A1;\n"
                                "mov rbx,rax \n"
                                "TYPE rbx \n"
  				"cmp rbx, T_INTEGER\n"
				"je .sobTrue\n"
				"mov rdi, .false\n"
				"jmp continue;\n"
				".sobTrue:\n"
				"mov rdi, .true\n"  ;; to check: .true and .false in section data
				"continue:\n"
                                "mov rax,0\n"
                                "call printf\n"
                                "leave\n"
				"ret;\n"
				) 'integer? fvar-table
		))
