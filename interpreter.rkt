#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         (lib "eopl.ss" "eopl"))

(define (drop-quotations str)
 (substring str 1 (- (string-length str) 1))
)

(define kp-lexer
  (lexer
   [(:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme))]
   ["+" (token-PLUS)]
   [";" (token-SEMICOLON)]
   ["==" (token-EQUAL)]
   ["<" (token-LT)]
   [">" (token-GT)]
   ["=" (token-ASSIGN)]
   ["!=" (token-NOT-EQUAL)]
   ["-" (token-MINUS)]
   ["*" (token-MULTIPLY)]
   ["/" (token-DIVIDE)]
   ["(" (token-OPEN-PAR)]
   [")" (token-CLOSE-PAR)]
   ["[" (token-OPEN-BRA)]
   ["]" (token-CLOSE-BRA)]
   ["," (token-COMMA)]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
      (case lexeme
	   [("while") (token-WHILE-key)]
	   [("do") (token-DO-key)]
	   [("end") (token-END-key)]
	   [("if") (token-IF-key)]
	   [("then") (token-THEN-key)]
	   [("else") (token-ELSE-key)]
	   [("return") (token-RETURN-key)]
	   [("None") (token-NULL-key)]
	   [("True") (token-TRUE-key)]
	   [("False") (token-FALSE-key)]
	   [else (token-VAR (string->symbol lexeme))]
	  )
   ]
   [(:: "\"" (complement (:: any-string "\"" any-string)) "\"") (token-STRING (drop-quotations lexeme))]
   [whitespace (kp-lexer input-port)]
   [(eof) (token-EOF)]
  )
)

(define-tokens group-a (NUM STRING VAR))
(define-empty-tokens group-b (EOF PLUS SEMICOLON EQUAL LT GT ASSIGN NOT-EQUAL MINUS MULTIPLY DIVIDE OPEN-PAR CLOSE-PAR OPEN-BRA CLOSE-BRA COMMA WHILE-key DO-key END-key IF-key THEN-key ELSE-key RETURN-key NULL-key TRUE-key FALSE-key))

;(define-datatype keyword keyword?)
;(define-datatype exp exp?)
;(define-datatype aexp aexp?)
;(define-datatype bexp bexp?)
;(define-datatype cexp cexp?)
;(define-datatype list-values list-values?)
;(define-datatype list-member list-member?)

(define-datatype command command?
 (single-keyword (kwd keyword?))
 (several-keywords (kwd keyword?) (cmd command?))
)

(define-datatype keyword keyword?
 (if-statement (condition exp?) (then-command command?) (else-command command?))
 (assignment-statement (lhs symbol?) (rhs exp?))
 (while-statement (condition exp?) (cmd command?))
 (return-statement (val exp?))
)

(define-datatype exp exp?
 (single-aexp (ae aexp?))
 (gt-exp (lhs aexp?) (rhs aexp?))
 (lt-exp (lhs aexp?) (rhs aexp?))
 (equal-exp (lhs aexp?) (rhs aexp?))
 (not-equal-exp (lhs aexp?) (rhs aexp?))
)

(define-datatype aexp aexp?
 (single-bexp (be bexp?))
 (minus-exp (lhs bexp?) (rhs aexp?))
 (plus-exp (lhs bexp?) (rhs aexp?))
)

(define-datatype bexp bexp?
 (single-cexp (ce cexp?))
 (multiply-exp (lhs cexp?) (rhs bexp?))
 (divide-exp (lhs cexp?) (rhs bexp?))
)

(define-datatype cexp cexp?
 (negative-cexp (nce cexp?))
 (exp-in-paras (e exp?))
 (pos-number (num number?))
 (null-exp)
 (variable (var symbol?))
 (true-exp)
 (false-exp)
 (string-exp (str string?))
 (empty-list)
 (non-empty-list (lvals list-values?))
 (list-extraction (var symbol?) (mem list-member?))
)

(define-datatype list-values list-values?
 (single-exp-in-list (e exp?))
 (several-exps-in-list (e exp?) (rest list-values?))
)

(define-datatype list-member list-member?
 (single-member (e exp?))
 (several-members (e exp?) (rest list-member?))
)

(define kp-parser
  (parser
   (start command-G)
   (end EOF)
   (error void)
   (tokens group-a group-b)
   (grammar
    (command-G
	 ((keyword-G) (single-keyword $1))
	 ((keyword-G SEMICOLON command-G) (several-keywords $1 $3))
	)
	(keyword-G 
	 ((IF-key exp-G THEN-key command-G ELSE-key command-G END-key) (if-statement $2 $4 $6))
	 ((VAR ASSIGN exp-G) (assignment-statement $1 $3))
	 ((WHILE-key exp-G DO-key command-G END-key) (while-statement $2 $4))
	 ((RETURN-key exp-G) (return-statement $2))
	)
	(exp-G
	 ((aexp-G) (single-aexp $1))
	 ((aexp-G GT aexp-G) (gt-exp $1 $3))
	 ((aexp-G LT aexp-G) (lt-exp $1 $3))
	 ((aexp-G EQUAL aexp-G) (equal-exp $1 $3))
	 ((aexp-G NOT-EQUAL aexp-G) (not-equal-exp $1 $3))
	)
	(aexp-G
	 ((bexp-G) (single-bexp $1))
	 ((bexp-G MINUS aexp-G) (minus-exp $1 $3))
	 ((bexp-G PLUS aexp-G) (plus-exp $1 $3))
	)
	(bexp-G
	 ((cexp-G) (single-cexp $1))
	 ((cexp-G MULTIPLY bexp-G) (multiply-exp $1 $3))
	 ((cexp-G DIVIDE bexp-G) (divide-exp $1 $3))
	)
	(cexp-G
	 ((MINUS cexp-G) (negative-cexp $2))
	 ((OPEN-PAR exp-G CLOSE-PAR) (exp-in-paras $2))
	 ((NUM) (pos-number $1))
	 ((NULL-key) (null-exp))
	 ((VAR) (variable $1))
	 ((TRUE-key) (true-exp))
	 ((FALSE-key) (false-exp))
	 ((STRING) (string-exp $1))
	 ((OPEN-BRA CLOSE-BRA) (empty-list))
	 ((OPEN-BRA list-values-G CLOSE-BRA) (non-empty-list $2))
	 ((VAR list-member-G) (list-extraction $1 $2))
	)
	(list-values-G
	 ((exp-G) (single-exp-in-list $1))
	 ((exp-G COMMA list-values-G) (several-exps-in-list $1 $3))
	)
	(list-member-G
	 ((OPEN-BRA exp-G CLOSE-BRA) (single-member $2))
	 ((OPEN-BRA exp-G CLOSE-BRA list-member-G) (several-members $2 $4))
	)
   )
  )
)

(define (empty-env)
 '()
)

(define (extend-env var val env) 
 (cons var (cons val env))
)

(define (apply-env var env)
 (cond
  [(null? env) 'error]
  [(eqv? (car env) var) (cadr env)]
  [else (apply-env var (cddr env))]
 )
)

(define (list-concat L1 L2)
 (cond
  [(null? L1) L2]
  [else (cons (car L1) (list-concat (cdr L1) L2))]
 )
)

(define (finished? env)
 (and (not (null? env)) (eqv? (car env) '_exit_val))
)

(define (run-while cond-expr cmd env)
 (cond
  [(finished? env) env]
  [(value-of-exp cond-expr env)
   (begin
    (define new-env (value-of-command cmd env))
	(run-while cond-expr cmd new-env)
   )
  ]
  [else env]
 )
)

(define (check-condition-on-list L value oper)
 (cond
  [(null? L) #t]
  [else (and (oper (car L) value) (check-condition-on-list (cdr L) value oper))]
 )
)

(define (check-condition lhs-exp rhs-exp env num-oper str-oper)
 (begin
  (define lhs (value-of-aexp lhs-exp env))
  (define rhs (value-of-aexp rhs-exp env))
  (cond
   [(and (list? lhs) (string? rhs)) (check-condition-on-list lhs rhs str-oper)]
   [(and (list? lhs) (number? rhs)) (check-condition-on-list lhs rhs num-oper)]
   [(and (string? lhs) (string? rhs)) (str-oper lhs rhs)]
   [(and (number? lhs) (number? rhs)) (num-oper lhs rhs)]
   [else 'error]
  )
 )
)

(define (check-equality lhs-exp rhs-exp env)
 (begin
  (define lhs (value-of-aexp lhs-exp env))
  (define rhs (value-of-aexp rhs-exp env))
  (equal? lhs rhs)
 )
)

(define (calculate-oper-on-list L value oper ltr)
 (cond
  [(null? L) '()]
  [else (cons (if ltr (oper (car L) value) (oper value (car L))) (calculate-oper-on-list (cdr L) value oper ltr))]
 )
)

(define (calculate-minus lhs-exp rhs-exp env)
 (begin
  (define lhs (value-of-bexp lhs-exp env))
  (define rhs (value-of-aexp rhs-exp env))
  (cond
   [(and (number? lhs) (number? rhs)) (- lhs rhs)]
   [(and (list? lhs) (number? rhs)) (calculate-oper-on-list lhs rhs - #t)]
   [(and (number? lhs) (list? rhs)) (calculate-oper-on-list rhs lhs - #f)]
   [else 'error]
  )
 )
)

(define (calculate-divide lhs-exp rhs-exp env)
 (begin
  (define lhs (value-of-cexp lhs-exp env))
  (define rhs (value-of-bexp rhs-exp env))
  (cond
   [(and (number? lhs) (number? rhs)) (/ lhs rhs)]
   [(and (list? lhs) (number? rhs)) (calculate-oper-on-list lhs rhs / #t)]
   [(and (number? lhs) (list? rhs)) (calculate-oper-on-list rhs lhs / #f)]
   [else 'error]
  )
 )
)

(define (calculate-plus lhs-exp rhs-exp env)
 (begin
  (define lhs (value-of-bexp lhs-exp env))
  (define rhs (value-of-aexp rhs-exp env))
  (cond
   [(and (number? lhs) (number? rhs)) (+ lhs rhs)]
   [(and (list? lhs) (number? rhs)) (calculate-oper-on-list lhs rhs + #t)]
   [(and (number? lhs) (list? rhs)) (calculate-oper-on-list rhs lhs + #f)]
   [(and (boolean? lhs) (boolean? rhs)) (or lhs rhs)]
   [(and (list? lhs) (boolean? rhs)) (calculate-oper-on-list lhs rhs (lambda (a b) (or a b)) #t)]
   [(and (boolean? lhs) (list? rhs)) (calculate-oper-on-list rhs lhs (lambda (a b) (or a b)) #f)]
   [(and (string? lhs) (string? rhs)) (string-append lhs rhs)]
   [(and (list? lhs) (string? rhs)) (calculate-oper-on-list lhs rhs string-append #t)]
   [(and (string? lhs) (list? rhs)) (calculate-oper-on-list rhs lhs string-append #f)]
   [(and (list? lhs) (list? rhs)) (list-concat lhs rhs)]
   [else 'error]
  )
 )
)

(define (calculate-multiply lhs-exp rhs-exp env)
 (begin
  (define lhs (value-of-cexp lhs-exp env))
  (define rhs (value-of-bexp rhs-exp env))
  (cond
   [(and (number? lhs) (number? rhs)) (* lhs rhs)]
   [(and (list? lhs) (number? rhs)) (calculate-oper-on-list lhs rhs * #t)]
   [(and (number? lhs) (list? rhs)) (calculate-oper-on-list rhs lhs * #f)]
   [(and (boolean? lhs) (boolean? rhs)) (and lhs rhs)]
   [(and (list? lhs) (boolean? rhs)) (calculate-oper-on-list lhs rhs (lambda (a b) (and a b)) #t)]
   [(and (boolean? lhs) (list? rhs)) (calculate-oper-on-list rhs lhs (lambda (a b) (and a b)) #f)]
   [else 'error]
  )
 )
)

(define (negative-value val)
 (cond
  [(number? val) (- val)]
  [(boolean? val) (not val)]
  [(list? val) (cond
    [(null? val) '()]
    [else (cons (negative-value (car val)) (negative-value (cdr val)))]
  )]
  [else 'error]
 )
)

(define (get-list-member L idx)
 (cond
  [(not (list? L)) 'error]
  [(null? L) 'error]
  [(zero? idx) (car L)]
  [else (get-list-member (cdr L) (- idx 1))]
 )
)

(define (value-of-command cmd env)
 (cases command cmd
  (single-keyword (kwd)
   (value-of-keyword kwd env)
  )
  (several-keywords (kwd rest-cmd)
   (begin
	(define new-env (value-of-keyword kwd env))
	(cond
	 [(finished? new-env) new-env]
	 [else (value-of-command rest-cmd new-env)]
	)
   )
  )
 )
)

(define (value-of-keyword kwd env)
 (cases keyword kwd
  (if-statement (cond-expr then-cmd else-cmd)
   (if (value-of-exp cond-expr env) (value-of-command then-cmd env) (value-of-command else-cmd env))
  )
  (assignment-statement (lhs rhs)
   (extend-env lhs (value-of-exp rhs env) env)
  )
  (while-statement (cond-expr cmd)
   (run-while cond-expr cmd env)
  )
  (return-statement (val) (extend-env '_exit_val (value-of-exp val env) env))
 )
)

(define (value-of-exp e env)
 (cases exp e
  (single-aexp (ae) (value-of-aexp ae env))
  (gt-exp (lhs rhs) (check-condition lhs rhs env > string>?))
  (lt-exp (lhs rhs) (check-condition lhs rhs env < string<?))
  (equal-exp (lhs rhs) (check-equality lhs rhs env))
  (not-equal-exp (lhs rhs) (not (check-equality lhs rhs env)))
 )
)

(define (value-of-aexp ae env)
 (cases aexp ae
  (single-bexp (be) (value-of-bexp be env))
  (minus-exp (lhs rhs) (calculate-minus lhs rhs env))
  (plus-exp (lhs rhs) (calculate-plus lhs rhs env))
 )
)

(define (value-of-bexp be env)
 (cases bexp be
  (single-cexp (ce) (value-of-cexp ce env))
  (multiply-exp (lhs rhs) (calculate-multiply lhs rhs env))
  (divide-exp (lhs rhs) (calculate-divide lhs rhs env))
 )
)

(define (value-of-cexp ce env)
 (cases cexp ce
  (negative-cexp (nce) (negative-value (value-of-cexp nce env)))
  (exp-in-paras (e) (value-of-exp e env))
  (pos-number (num) num)
  (null-exp () 'None)
  (variable (var) (apply-env var env))
  (true-exp () #t)
  (false-exp () #f)
  (string-exp (str) str)
  (empty-list () '())
  (non-empty-list (lvals) (value-of-list-values lvals env))
  (list-extraction (var mem) (value-of-list-member (apply-env var env) mem env))
 )
)

(define (value-of-list-values lvals env)
 (cases list-values lvals
  (single-exp-in-list (e) (cons (value-of-exp e env) '()))
  (several-exps-in-list (e rest) (cons (value-of-exp e env) (value-of-list-values rest env)))
 )
)

(define (value-of-list-member L mem env)
 (cases list-member mem
  (single-member (e) (get-list-member L (value-of-exp e env)))
  (several-members (e rest) (value-of-list-member (get-list-member L (value-of-exp e env)) rest env))
 )
)

(define (print-tokens custom-lexer last-token)
 (cond
  [(equal? last-token 'EOF) (display "---")]
  [else (begin
    (define new-token (custom-lexer))
	(display new-token)
	(display "\n")
	(print-tokens custom-lexer new-token)
  )]
 )
)

(define (run-program input-port)
 (begin
  (define lex-this (lambda (lexer input) (lambda () (lexer input))))
  (define my-lexer (lex-this kp-lexer input-port))
; (print-tokens my-lexer " ")
  (define parser-res (kp-parser my-lexer))
  (define final-env (value-of-command parser-res (empty-env)))
  (display (apply-env '_exit_val final-env))
 )
)

(define (evaluate filepath)
 (run-program (open-input-file filepath))
)

;(evaluate "code.myc")

;(run-program (open-input-string "fib = [1, 1, 2, 3, 5, 8, 13]; c = 0; sum = 0; while c < 7 do sum = sum + fib[c]; c = c + 1 end; return sum"))
;(run-program (open-input-string "x = [[\"salam\", None], false]; return x[0][1] == None"))
;(run-program (open-input-string "x = -[[true, 1], false]"))
(run-program (open-input-string "x = False; return x"))
;(run-program (open-input-string "x = [1, 2]; y = [3, 4]; z = y + x"))
;(run-program (open-input-string "x = \"hi\"; y = x + x"))
;(run-program (open-input-string "x = true + [false, true]"))
;(run-program (open-input-string "x = true * [false, true]"))
;(run-program (open-input-string "x = \"yes\" + [\"1\", \"2\", \"3\"] + \"no\""))
;(run-program (open-input-string "a = 4; x = 10 - [1, 2, 3, a]"))
;(run-program (open-input-string "if [\"x\", \"z\"] > \"h\" then return true else return false end"))
;(run-program (open-input-string "if [1, 12, 3] < 10 then return \"salam\" else return 2 end"))
;(run-program (open-input-string "x = 10; arr = [1, 5, 3]; x = 11; return arr"))
;(run-program (open-input-string "c = false; while c do c = false; return 4 end; return 12"))
;(run-program (open-input-string "if false then return 12 else c = 4; return 13 end"))

