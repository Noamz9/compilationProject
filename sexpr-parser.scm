(load "pc.scm")



;-------------------------------------------------IgnoreSpaceOrComments

(define <IgnoreSpaceOrComments>
  (new
  ;whitespace
  (*parser (range #\nul #\space))
  *star
 
  ;ExpComment
   (*parser (word "#;"))
   (*parser (range #\nul #\space))
   *star
   (*delayed (lambda () <InfixAddOrSub>))
   (*delayed (lambda () <sexpr>))
   (*disj 2)
   (*parser (range #\nul #\space))
   *star
   (*caten 4)
   (*pack-with (lambda (pre s1 exp s2) (list)))
   
  ;LineComment
   (*parser (char #\;))
   (*parser <any-char>)
   (*parser (char #\newline))
   (*parser <end-of-input>)
   (*disj 2)
   *diff
   *star
   (*parser (char #\newline))
   (*parser <end-of-input>)
   (*disj 2)
   (*caten 3)
   (*pack-with (lambda (pre exp end) (list)))

  (*disj 2)

  ;whitespace
  (*parser (range #\nul #\space))
  *star
  (*caten 2)
  *star
  (*caten 2)


  done))





;-------------------------------------------------boolean
(define <Boolean>
  (new
   (*parser (word-ci "#t"))
   (*pack (lambda (_) #t)) 
   (*parser (word-ci "#f"))
   (*pack (lambda (_) #f)) 

   (*disj 2)
   
   (*delayed (lambda () <Symbol>))
   (*delayed (lambda () <Number>))
   (*disj 2)

   *not-followed-by
   
   done))

;-------------------------------------------------char
(define <VisibleSimpleChar>
  (new
   (*parser (range #\! #\rubout))
   (*delayed (lambda () <Number>))
   (*delayed (lambda () <Symbol>))
   (*disj 2)
   *not-followed-by

   done))

(define <NamedChar>
  (new
   (*parser (word-ci "lambda"))
   (*pack (lambda (lambda) (integer->char 955)))
   (*parser (word-ci "newline"))
   (*pack (lambda (newline) #\newline))
   (*parser (word-ci "nul"))
   (*pack (lambda (nul) #\nul))
   (*parser (word-ci "page"))
   (*pack (lambda (page) #\page))
   (*parser (word-ci "return"))
   (*pack (lambda (return) #\return))
   (*parser (word-ci "space"))
   (*pack (lambda (space) #\space))
   (*parser (word-ci "tab"))
   (*pack (lambda (tab) #\tab))
   
   (*disj 7)
   done))

(define <HexChar>
  (new
   (*parser (range-ci #\0 #\9))
   (*parser (range-ci #\a #\f))
   (*disj 2)
   done))


(define <HexUnicodeChar>
  (new
   (*parser (char-ci #\x))
   (*parser <HexChar>) *plus
   (*caten 2)
   (*pack-with (lambda (ch-p hexchar) 
                       (string->number
                           (list->string hexchar) 16)))
   (*pack (lambda (decnum) (integer->char decnum)))
            
    done))
   

(define <CharPrefix>
  (new
   (*parser (word "#\\"))

   done))

(define <Char>
  (new
  (*parser <CharPrefix>)
  (*parser <VisibleSimpleChar>)
  (*parser <NamedChar>)
  (*parser <HexUnicodeChar>)
   
   (*disj 3)
   (*caten 2)
   (*pack-with (lambda (ch_p ch) ch)) 
   
   done))


;------------------------------------------------------number
(define <Digit1-9>
  (new
    (*parser (range #\1 #\9))
   done))

(define <Digit0-9>
  (new
   (*parser (range #\0 #\9))
   done))

(define <Natural>
  (new
   (*parser (char #\0))
   (*pack (lambda(_) 0))
   
   (*parser <Digit1-9>)        
   (*parser <Digit0-9>) *star
   (*caten 2)
   (*pack-with
    (lambda (first rest)
      (string->number
       (list->string
        (cons first rest)))))
   (*disj 2)
   done))

(define <Integer>
  (new
   (*parser (char #\+))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda (plus num) 
                       num))
   
   (*parser (char #\-))
   (*parser <Natural>)
   (*caten 2)
   (*pack-with (lambda(minus num) (* -1 num)))

   (*parser <Natural>)
   
   (*disj 3)

   (*parser (range-ci #\a #\z))
   *not-followed-by
   
   done))

(define <Fraction>
  (new
   (*parser <Integer>)
   (*parser (char #\/))
   (*parser <Natural>)

   (*caten 3)
   (*pack-with (lambda (num1 d num2)
                (/ num1 num2)))
   done))


(define <Number>
  (new
   
   (*parser <Fraction>)
   (*parser <Integer>)

   (*disj 2)

  done))

;------------------------------------------------------string

(define <StringHexChar>
  (new
   (*parser (char #\\))
   (*parser (char #\x))
   (*parser <HexChar>) *plus
   (*parser (char #\;))
   (*caten 4)
   (*pack-with (lambda (ch-p x-p hexchar np) 
                       (string->number
                           (list->string hexchar) 16)))
   (*pack (lambda (decnum) (integer->char decnum)))
   done))

(define <StringLiteralChar>
  (new
   (*parser <any-char>)
   (*parser (char #\\))
   *diff
   (*parser (char #\"))
   *diff
   (*pack (lambda (ch)
         ch))
   done))

(define <StringMetaChar>
  (new
   (*parser (word "\\\\"))
   (*pack (lambda (_) #\\))
   (*parser (word "\\\""))
   (*pack (lambda (_) #\"))
   (*parser (word "\\t"))
   (*pack (lambda (_) #\tab))
   (*parser (word "\\n"))
   (*pack (lambda (_) #\newline))
   (*parser (word "\\f"))
   (*pack (lambda (_) #\page))
   (*parser (word "\\r"))
   (*pack (lambda (_) #\return))
   (*disj 6)
   done))





(define <StringChar>
  (new
   (*parser <StringMetaChar>)
   (*parser <StringHexChar>)
   (*parser <StringLiteralChar>)

   (*disj 3)
   done))

(define <String>
  (new
   (*parser (word "\""))
   (*parser <StringChar>) *star
   (*parser (word "\""))

   (*caten 3)
   (*pack-with (lambda (d1 str d2)
              (list->string str)))
   done))

;------------------------------------------------------symbol

(define <SymbolChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range #\a #\z))
   (*parser (range #\A #\Z))
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\-))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\+))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*parser (char #\/))
  
   (*disj 15)    
   done))

(define <Symbol>
  (new
   (*parser <SymbolChar>) *plus
   (*pack (lambda (sym)
            (string->symbol
              (string-downcase (list->string sym)))))

   done))

;------------------------------------------------------proper list

(define <ProperList>
  (new
    (*parser (char #\( ))
      (*parser <IgnoreSpaceOrComments>)
    (*delayed (lambda () <sexpr>)) 
    *star
      (*parser <IgnoreSpaceOrComments>)
    (*parser (char #\) ))
    (*caten 5)

    (*pack-with (lambda (o s1 sexp s c)
                          sexp))

   done))

;------------------------------------------------------improper list

(define <ImproperList>
  (new
    (*parser (char #\( ))
      (*parser <IgnoreSpaceOrComments>)
    (*delayed (lambda () <sexpr>)) 
    *plus
      (*parser <IgnoreSpaceOrComments>)
    (*parser (char #\. ))
      (*parser <IgnoreSpaceOrComments>)
    (*delayed (lambda () <sexpr>))
      (*parser <IgnoreSpaceOrComments>)
    (*parser (char #\) ))
    (*caten 9)

    (*pack-with (lambda (o s1 sexps s2 dot s3 sexp s4 c)
                         `(,@sexps . ,sexp)))

    done))

;------------------------------------------------------vector

(define <Vector>
  (new
    (*parser (char #\#))
      (*parser <IgnoreSpaceOrComments>)
    (*parser (char #\())
      (*parser <IgnoreSpaceOrComments>)
    (*delayed (lambda () <sexpr>)) *star
      (*parser <IgnoreSpaceOrComments>)
    (*parser (char #\)))
    (*caten 7)

    (*pack-with (lambda (s s1 o s2 sexp s3 c)
                          `#(,@sexp)))

    done))


;------------------------------------------------------quoted

(define <Quoted>
  (new
    (*parser (char #\'))
    (*parser <IgnoreSpaceOrComments>)
    (*delayed (lambda () <sexpr>))
    (*parser <IgnoreSpaceOrComments>)
    (*caten 4)

   (*pack-with (lambda (p s1 exp s2) (list 'quote exp)))
    done))

  


(define <QuasiQuoted>
  (new
   (*parser (char #\` ))
   (*parser <IgnoreSpaceOrComments>)
   (*delayed (lambda () <sexpr>))
   (*parser <IgnoreSpaceOrComments>)
   (*caten 4)

   (*pack-with (lambda (p s1 exp s2) (list 'quasiquote exp)))

   done))


(define <Unquoted>
   (new
    (*parser (char #\,))
    (*parser (char #\@))
    *not-followed-by
    (*parser <IgnoreSpaceOrComments>)
    (*delayed (lambda () <sexpr>))
    (*parser <IgnoreSpaceOrComments>)
    (*caten 4)
    (*pack-with (lambda (p s1 exp s2) (list 'unquote exp)))
      done))

   

(define <UnquoteAndSpliced>
  (new
   (*parser (char #\, ))
   (*parser (char #\@ ))
   (*parser <IgnoreSpaceOrComments>)
   (*delayed (lambda () <sexpr>))
   (*parser <IgnoreSpaceOrComments>)
   (*caten 5)

   (*pack-with (lambda (p s s1 exp s2) (list 'unquote-splicing exp)))

    done))

;---------------------------------------------------------------call by name


(define <CBNameSyntax2>
  (new
  (*parser (char #\{))
   (*parser <IgnoreSpaceOrComments>)
  (*delayed (lambda () <sexpr>))
   (*parser <IgnoreSpaceOrComments>)
  (*parser (char #\}))
  (*caten 5)

  (*pack-with (lambda (o s1 sexp s2 c)
                          (list 'cbname sexp))) 

  done))


(define <CBNameSyntax1>
  (new
  (*parser (char #\@))
    (*parser <IgnoreSpaceOrComments>)
  (*delayed (lambda () <sexpr>))
    (*parser <IgnoreSpaceOrComments>)
  (*caten 4)

  (*pack-with (lambda (q s1 sexp s2)
                         (list 'cbname sexp))) ;fix the pack

 
  done))


(define <CBName>
  (new 
    (*parser <CBNameSyntax1>)
    (*parser <CBNameSyntax2>)
    (*disj 2)
      done))



;infix----------------------------------------------------------------
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
(define <InfixPrefixExtensionPrefix>
   (new
    (*parser (word "##"))
    (*parser (word "#%"))
    (*disj 2)
    done))

(define <InfixSymbol>
   (new
   (*parser (range #\0 #\9))
   (*parser (range-ci #\a #\z))
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
  
   (*disj 9)
   *plus

   (*pack (lambda (symbol)
            (if (list? symbol) 
               (string->symbol
                (string-downcase
                  (list->string symbol))) 
                    (string->symbol (string-downcase (string symbol))))))
           


   
    done))


(define <InfixParen>
 (new  

   (*parser (char #\())
   (*parser <IgnoreSpaceOrComments>) 
   (*delayed (lambda ()  <InfixAddOrSub>))
   (*parser <IgnoreSpaceOrComments>) 
   (*parser (char #\)))
   (*parser <IgnoreSpaceOrComments>) 
   (*caten 6)
   (*pack-with (lambda (o s1 exp s2 c s3) exp))
   done))


  


(define <InfixStopExpression>
(new
  ;number
  (*parser <Number>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (num s1) num))

  ;symbol
  (*parser <InfixSymbol>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (sym s1) sym))

  ;paren
  (*parser <InfixParen>)

  ;escape
  (*parser <InfixPrefixExtensionPrefix>)
  (*delayed (lambda () <sexpr>))
  (*caten 2)
  (*pack-with (lambda (pre sexp)
                      sexp))

  (*disj 4)

      done))




(define <InfixArgList>
 (new
   (*delayed (lambda () <InfixAddOrSub>))
   (*parser <IgnoreSpaceOrComments>)
   (*caten 2)
   (*pack-with (lambda (arg s1)
                      arg))

   (*parser (char #\,))
   (*parser <IgnoreSpaceOrComments>)
   (*delayed (lambda () <InfixAddOrSub>))
   (*parser <IgnoreSpaceOrComments>)
   (*caten 4) 
   (*pack-with (lambda (p s1 arg s2) arg))
   *star
   (*caten 2)
   (*pack-with cons)

   (*parser <epsilon>)
   (*disj 2)

    done))


(define <arr+func+neg+escape>
  (new
  
                   
  ;func
  (*parser <InfixStopExpression>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (name s1)
                 name))

  (*parser (char #\())
  (*parser <IgnoreSpaceOrComments>)
  (*parser  <InfixArgList>)
  (*parser <IgnoreSpaceOrComments>)
  (*parser (char #\)))
  (*caten 5)
  (*pack-with (lambda (o s1 args s2 c) 
            args))

  (*caten 2)
  (*pack-with (lambda (name args) 
            (append (list name) args)))

  ;array
  (*parser   <InfixStopExpression>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (arg1 s1)
                 arg1))

  (*parser (char #\[))
  (*delayed (lambda ()  <InfixAddOrSub>))
  (*parser (char #\]))
  (*caten 3)
  (*pack-with (lambda (o exp c)
                      exp))
  *plus
  (*caten 2)

  (*pack-with (lambda (vector ref) 
          (fold-left (lambda (m1 m2) (list 'vector-ref m1 m2)) vector ref)))
  




  ;atomic
  (*parser <InfixStopExpression>)

  (*disj 3)
   
   
  done))


(define <Power>
    (new
          (*parser (char #\^))
          (*parser (word "**"))
          (*disj 2)
  done))


(define <InfixPow>
  (new

  (*parser  <arr+func+neg+escape>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (arg1 s1)
                 arg1))

  (*parser <Power>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (op s1)
                 op))

  (*parser  <arr+func+neg+escape>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (arg2 s1)
                 arg2))

  (*caten 2)
  (*pack-with (lambda (pow arg2)
                 arg2))
  *star
  (*caten 2)

  (*pack-with (lambda (exp lst) 
         (fold-left (lambda (x y) (list 'expt y x))
            (car (reverse (cons exp lst))) (cdr (reverse (cons exp lst))))))
    done))



(define <MulOrDiv>
  (new
   (*parser (char #\*))
   (*parser (char #\/))
   (*disj 2)
   (*pack (lambda (sym)
            (string->symbol
              (list->string (list sym)))))
   done))



(define <InfixMulOrDiv>
(new

  (*parser <InfixPow>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (arg1 s1)
                 arg1))

  (*parser <MulOrDiv>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (op s1)
                 op))

  (*parser <InfixPow>)
  (*parser <IgnoreSpaceOrComments>)
  (*caten 2)
  (*pack-with (lambda (arg2 s1)
                 arg2))

  (*caten 2)
  *star
  (*caten 2)

  (*pack-with (lambda (n1 n2)
              (if (null? n2) n1
                  `,(fold-left (lambda (m1 m2) `(,(car m2) ,m1 ,(cadr m2))) n1 n2))))
     done))

(define <AddOrSub>
  (new

   (*parser (char #\+))
   (*parser (char #\-))
   (*disj 2)
   (*pack (lambda (sym)
            (string->symbol
              (list->string (list sym)))))
   done))

(define <InfixAddOrSub>
(new

   (*parser  <InfixMulOrDiv>)
   (*parser <IgnoreSpaceOrComments>)
   (*caten 2)
   (*pack-with (lambda (arg1 s1)
                 arg1))

   (*parser  <AddOrSub>)
   (*parser <IgnoreSpaceOrComments>)
   (*caten 2)
   (*pack-with (lambda (op s1)
                 op))

   (*parser  <InfixMulOrDiv>)
   (*parser <IgnoreSpaceOrComments>)
   (*caten 2)
   (*pack-with (lambda (arg2 s1)
                 arg2))

   (*caten 2)       
   *star
   (*caten 2)

   (*pack-with (lambda (n1 n2)
              (if (null? n2) n1
                  `,(fold-left (lambda (m1 m2) `(,(car m2) ,m1 ,(cadr m2))) n1 n2))))

     ;neg
  (*parser (char #\-))
  (*delayed (lambda () <InfixStopExpression>))
  (*caten 2)
  (*pack-with (lambda (minus exp) minus exp))

  (*disj 2)
    done))





(define <InfixExtension>
  (new
(*parser <IgnoreSpaceOrComments>)
  (*parser <InfixPrefixExtensionPrefix>)
  (*parser <IgnoreSpaceOrComments>)
  (*parser <InfixAddOrSub>)
  (*parser <IgnoreSpaceOrComments>)

  (*caten 5)

  (*pack-with (lambda (s0 pre s1 ext s2)
                 ext))


done))





(define <sexpr>
  (new

   (*parser <IgnoreSpaceOrComments>)

   (*parser <Boolean>)
   (*parser <Char>)
   (*parser <Number>)
   (*parser <String>)
   (*parser <Symbol>)
   (*parser <ProperList>)
   (*parser <ImproperList>)
   (*parser <Vector>)
   (*parser <Quoted>)
   (*parser <QuasiQuoted>)
   (*parser <Unquoted>)
   (*parser <UnquoteAndSpliced>)
   (*parser <CBName>)
   (*parser <InfixExtension>)
   (*disj 14)

   (*parser <IgnoreSpaceOrComments>)

   (*caten 3)
   (*pack-with (lambda (s1 sexp s2)
                  sexp))

   
   done))

