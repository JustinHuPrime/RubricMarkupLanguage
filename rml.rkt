#!/usr/bin/env racket
#lang racket

(require racket/cmdline)

(define out-filename (make-parameter null))
(define in-filename (make-parameter null))
(define debug (make-parameter #f))

(define (DEBUG-PROBE x)
  (displayln (format "DEBUG: ~s" x) (current-error-port))
  x)

(define (blank-line? line)
  (andmap char-whitespace? (string->list line)))

;; values are either Numbers, Booleans, Strings, or Grades
(struct grade (value out-of))

(struct locatable (line character) #:transparent)

(struct token locatable () #:transparent)
(struct plain-text token (text) #:transparent)
(define KEYWORDS '("config"
                   "newline_collapse"
                   "input"
                   "prompt"
                   "to"
                   "if"
                   "else"
                   "let"
                   "floor"
                   "ceil"
                   "round"
                   "boolean"
                   "number"
                   "string"
                   "grade"
                   "true"
                   "false"))
(define CONFIG-OPTIONS (hash "newline_collapse" #t))
(struct keyword token (name) #:transparent)
(struct punctuation token (name) #:transparent)
(struct identifier token (name) #:transparent)
(struct literal token (value) #:transparent)

(struct ast locatable () #:transparent)
;; string, expression
(struct config-stmt ast (config-name value) #:transparent)
;; type, expression, string
(struct input-stmt ast (input-type prompt name) #:transparent)
;; type, expression, string, expression, expression
(struct conditional-input-stmt ast (input-type prompt name predicate alternative) #:transparent)
;; string, expression
(struct let-stmt ast (name value) #:transparent)

;; (listof expression literal identifier)
(struct line ast (exprs) #:transparent)
;; expression expression expression
(struct ternary ast (predicate consequent alternative) #:transparent)
(define LOGICAL-OPERATORS '("&&"
                            "||"))
(define EQUALITY-OPERATORS '("=="
                             "!="))
(define COMPARISON-OPERATORS '("<"
                               "<="
                               ">"
                               ">="))
(define ADDITION-OPERATORS '("+"
                             "-"))
(define MULTIPLICATION-OPERATORS '("*"
                                   "/"
                                   "%"
                                   "//"))
;; string expression expression
(struct binary ast (operator lhs rhs) #:transparent)
(define PREFIX-OPERATORS '("-"
                           "!"))
(define PRIMARY-OPERATORS '("floor"
                            "ceil"
                            "round"))
;; string expression
(struct unary ast (operator target) #:transparent)

(define SIMPLE-TYPE-KEYWORDS '("boolean"
                               "number"
                               "string"))
;; string
(struct keyword-type ast (name) #:transparent)
;; expression
(struct grade-type ast (out-of) #:transparent)

(define (error/nums line character format-string . format-args)
  (raise (exn:fail:user (apply format
                               (string-append "~a:~a:~a: " format-string)
                               (in-filename)
                               line
                               character
                               format-args)
                        (current-continuation-marks))))
(define (error/locatable locatable format-string . format-args)
  (apply error/nums
         (locatable-line locatable)
         (locatable-character locatable)
         format-string
         format-args))

;; (listof character) number number -> (listof character) number (or/c token #f)
(define (tokenize/one/code chars line-num char-num)
  (define (tokenize/word chars line-num char-num)
    (let loop ([chars chars]
               [rsf '()])
      (cond [(null? chars)
             (values '()
                     (+ char-num (length rsf))
                     (let ([word (list->string (reverse rsf))])
                       (if (member word KEYWORDS)
                           (keyword line-num char-num word)
                           (identifier line-num char-num word))))]
            [else (if (or (char=? (car chars) #\_)
                          (char-alphabetic? (car chars))
                          (char-numeric? (car chars)))
                      (loop (cdr chars) (cons (car chars) rsf))
                      (let ([word (list->string (reverse rsf))])
                        (values chars
                                (+ char-num (string-length word))
                                (if (member word KEYWORDS)
                                    (keyword line-num char-num word)
                                    (identifier line-num char-num word)))))])))
  (define (tokenize/punctuation chars line-num char-num)
    (match chars
      [(list* #\& #\& rest) (values rest (+ char-num 2) (punctuation line-num char-num "&&"))]
      [(list* #\| #\| rest) (values rest (+ char-num 2) (punctuation line-num char-num "||"))]
      [(list* #\= #\= rest) (values rest (+ char-num 2) (punctuation line-num char-num "=="))]
      [(list* #\! #\= rest) (values rest (+ char-num 2) (punctuation line-num char-num "!="))]
      [(list* #\< #\= rest) (values rest (+ char-num 2) (punctuation line-num char-num "<="))]
      [(list* #\> #\= rest) (values rest (+ char-num 2) (punctuation line-num char-num ">="))]
      [(list* #\/ #\/ rest) (values rest (+ char-num 2) (punctuation line-num char-num "//"))]
      [(list* #\? rest) (values rest (+ char-num 1) (punctuation line-num char-num "?"))]
      [(list* #\: rest) (values rest (+ char-num 1) (punctuation line-num char-num ":"))]
      [(list* #\= rest) (values rest (+ char-num 1) (punctuation line-num char-num "="))]
      [(list* #\! rest) (values rest (+ char-num 1) (punctuation line-num char-num "!"))]
      [(list* #\< rest) (values rest (+ char-num 1) (punctuation line-num char-num "<"))]
      [(list* #\> rest) (values rest (+ char-num 1) (punctuation line-num char-num ">"))]
      [(list* #\+ rest) (values rest (+ char-num 1) (punctuation line-num char-num "+"))]
      [(list* #\- rest) (values rest (+ char-num 1) (punctuation line-num char-num "-"))]
      [(list* #\* rest) (values rest (+ char-num 1) (punctuation line-num char-num "*"))]
      [(list* #\/ rest) (values rest (+ char-num 1) (punctuation line-num char-num "/"))]
      [(list* #\% rest) (values rest (+ char-num 1) (punctuation line-num char-num "%"))]
      [(list* #\( rest) (values rest (+ char-num 1) (punctuation line-num char-num "("))]
      [(list* #\) rest) (values rest (+ char-num 1) (punctuation line-num char-num ")"))]
      [(list* c rest) (error/nums line-num char-num "unexpected character '~a'" c)]))
  (define (tokenize/string chars line-num char-num)
    (let loop ([chars (cdr chars)]
               [rsf '()]
               [length-adjustment 0])
      (cond [(null? chars) (error/nums line-num char-num "unterminated string literal")]
            [else (match chars
                    [(list* #\" rest) (values rest
                                              (+ char-num 2 length-adjustment (length rsf))
                                              (literal line-num char-num (list->string (reverse rsf))))]
                    [(list* #\\ #\n rest) (loop rest (cons #\newline rsf) (add1 length-adjustment))]
                    [(list* #\\ #\t rest) (loop rest (cons #\tab rsf) (add1 length-adjustment))]
                    [(list* #\\ #\" rest) (loop rest (cons #\" rsf) (add1 length-adjustment))]
                    [(list* #\\ #\\ rest) (loop rest (cons #\\ rsf) (add1 length-adjustment))]
                    [(list* #\\ rest) (error/nums line-num
                                                  (+ char-num 1 length-adjustment (length rsf))
                                                  "unrecognized escape sequence")]
                    [(list* c rest) (loop rest (cons c rsf) length-adjustment)])])))
  (define (tokenize/number chars line-num char-num)
    (let loop ([chars chars]
               [rsf '()]
               [seen-decimal #f])
      (cond [(null? chars) (values '()
                                   (+ char-num (length rsf))
                                   (literal line-num char-num (string->number (list->string (reverse rsf)))))]
            [else (cond [(and seen-decimal (char=? #\. (car chars)))
                         (error/nums line-num (+ char-num (length rsf)) "unexpected decimal point")]
                        [(char=? #\. (car chars))
                         (loop (cdr chars) (cons #\. rsf) #t)]
                        [(char-numeric? (car chars))
                         (loop (cdr chars) (cons (car chars) rsf) seen-decimal)]
                        [else
                         (values chars
                                 (+ char-num (length rsf))
                                 (literal line-num char-num (string->number (list->string (reverse rsf)))))])])))
    
  (cond [(null? chars) (values '() char-num #f)]
        [else (cond [(char-whitespace? (car chars))
                     (tokenize/one/code (cdr chars) line-num (add1 char-num))]
                    [(or (char-alphabetic? (car chars))
                         (char=? (car chars) #\_))
                     (tokenize/word chars line-num char-num)]
                    [(char=? (car chars) #\")
                     (tokenize/string chars line-num char-num)]
                    [(or (char-symbolic? (car chars))
                         (char-punctuation? (car chars)))
                     (tokenize/punctuation chars line-num char-num)]
                    [(char-numeric? (car chars))
                     (tokenize/number chars line-num char-num)]
                    [else (error/nums line-num char-num "unexpected character '~a'" (car chars))])]))

;; (listof character) number number -> (listof-character) number (or/c token (listof token) #f)
(define (tokenize/one/body chars line-num char-num)
  (define (tokenize/string chars line-num char-num)
    (let loop ([chars (cdr chars)]
               [rsf '()]
               [length-adjustment 0])
      (cond [(null? chars) (error/nums line-num char-num "unterminated string literal")]
            [else (match chars
                    [(list* #\" rest) (let ([str (list->string (reverse rsf))])
                                        (values rest
                                                (+ char-num 2 length-adjustment (string-length str))
                                                (literal line-num char-num str)))]
                    [(list* #\\ #\n rest) (loop rest (cons #\newline rsf) (add1 length-adjustment))]
                    [(list* #\\ #\t rest) (loop rest (cons #\tab rsf) (add1 length-adjustment))]
                    [(list* #\\ #\" rest) (loop rest (cons #\" rsf) (add1 length-adjustment))]
                    [(list* #\\ #\\ rest) (loop rest (cons #\\ rsf) (add1 length-adjustment))]
                    [(list* #\\ rest) (error/nums line-num
                                                  (+ char-num 1 length-adjustment (length rsf))
                                                  "unrecognized escape sequence")]
                    [(list* c rest) (loop (cdr chars) (cons c rsf) length-adjustment)])])))
  (define (tokenize/plain-text chars line-num char-num)
    (let loop ([chars chars]
               [rsf '()]
               [length-adjustment 0])
      (cond [(null? chars) (values '()
                                   (+ char-num length-adjustment (length rsf))
                                   (plain-text line-num char-num (list->string (reverse rsf))))]
            [else (match chars
                    [(list* #\$ #\$ rest) (loop rest (cons #\$ rsf) (add1 length-adjustment))]
                    [(list* #\$ #\{ rest) (values chars
                                                  (+ char-num length-adjustment (length rsf))
                                                  (plain-text line-num char-num (list->string (reverse rsf))))]
                    [(list* c rest) (loop rest (cons c rsf) length-adjustment)])])))
  
  (cond [(null? chars) (values '() char-num #f)]
        [else (match chars
                [(list* #\$ #\{ rest) (let ([code-length (index-of rest #\})])
                                            (if code-length
                                                (let ([code-string (list->string (take rest code-length))]
                                                      [remainder (drop rest (+ code-length 1))])
                                                  (values remainder
                                                          (+ char-num 3 code-length)
                                                          (tokenize code-string line-num (+ char-num 2) tokenize/one/code)))
                                                (error/nums line-num
                                                            char-num 
                                                            "unterminated code block")))]
                [_ (tokenize/plain-text chars line-num char-num)])]))

;; string number ((listof character) number number -> (listof-character) number (or/c token (listof token) #f)) -> (listof token)
(define (tokenize line line-num char-num tokenizer/one)
  (let loop ([chars (string->list line)]
             [char-num char-num]
             [rsf '()])
    (cond [(null? chars) (reverse rsf)]
          [else (let-values ([(next-chars next-char-num token) (tokenizer/one chars line-num char-num)])
                  (loop next-chars
                        next-char-num
                        (cond [(token? token) (cons token rsf)]
                              [(list? token) (append token rsf)]
                              [else rsf])))])))

;; (listof string) -> (listof string) (listof number)
(define (stmt-lines-only lines)
  (let loop ([rsf-lines '()]
             [rsf-line-nums '()]
             [lines lines]
             [line-nums (build-list (length lines) add1)])
    (cond [(null? lines) (values (reverse rsf-lines)
                                 (reverse rsf-line-nums))]
          [else (cond [(blank-line? (car lines))
                       (loop rsf-lines (cdr lines) (cdr line-nums))]
                      [(string-prefix? (car lines) "$#")
                       (loop rsf-lines (cdr lines) (cdr line-nums))]
                      [(string-prefix? (car lines) "$")
                       (loop (cons (car lines) rsf-lines) (cons (car line-nums) rsf-line-nums) (cdr lines) (cdr line-nums))]
                      [else (values (reverse rsf-lines)
                                    (reverse rsf-line-nums))])])))

;; (listof token) -> (listof token) ast
(define (parse/expression tokens line0)
  (define (parse/primary tokens)
    (cond [(null? tokens)
           (error/nums line0 0 "unexpected end of line")]
          [(identifier? (car tokens))
           (values (cdr tokens)
                   (car tokens))]
          [(literal? (car tokens))
           (values (cdr tokens)
                   (car tokens))]
          [(and (punctuation? (car tokens)) (string=? (punctuation-name (car tokens)) "("))
           (let-values ([(tokens expression) (parse/expression tokens line0)])
             (cond [(null? tokens)
                    (error/locatable expression "expected a close paren")]
                   [(and (punctuation? (car tokens)) (string=? (punctuation-name (car tokens)) ")"))
                    (values (cdr tokens)
                            expression)]
                   [else
                    (error/locatable (car tokens) "unexpected token")]))]
          [(and (keyword? (car tokens)) (member (keyword-name (car tokens)) PRIMARY-OPERATORS))
           (let ([operator (car tokens)]
                 [tokens (cdr tokens)])
             (cond [(null? tokens)
                    (error/locatable operator "expected an open paren after this")]
                   [(and (punctuation? (car tokens)) (string=? (punctuation-name (car tokens)) "("))
                    (let-values ([(tokens target) (parse/expression tokens line0)])
                      (cond [(null? tokens)
                             (error/locatable target "expected a close paren")]
                            [(and (punctuation? (car tokens)) (string=? (punctuation-name (car tokens)) ")"))
                             (values (cdr tokens)
                                     (unary (locatable-line operator)
                                            (locatable-character operator)
                                            (keyword-name operator)
                                            target))]
                            [else
                             (error/locatable (car tokens) "unexpected token")]))]
                   [else
                    (error/locatable (car tokens) "unexpected token")]))]
          [else
           (error/locatable (car tokens) "unexpected token")]))
  (define (parse/prefix tokens)
    (cond [(and (not (null? tokens)) (punctuation? (car tokens)) (member (punctuation-name (car tokens) PREFIX-OPERATORS)))
           (let ([operator (car tokens)]
                 [tokens (cdr tokens)])
             (cond [(null? tokens)
                    (error/locatable operator "expected an expresison after this")]
                   [else
                    (let-values ([(remainder target) (parse/prefix tokens)])
                      (values (unary (locatable-line operator)
                                     (locatable-character operator)
                                     (punctuation-name operator)
                                     target)))]))]
          [else
           (parse/primary tokens)]))
  (define (parse/binary tokens operators sub-parser)
    (let-values ([(tokens base) (sub-parser tokens)])
      (let loop ([tokens tokens]
                 [base base])
        (cond [(and (not (null? tokens)) (punctuation? (car tokens)) (member (punctuation-name (car tokens)) operators))
               (let ([operator (car tokens)]
                     [tokens (cdr tokens)])
                 (cond [(null? tokens)
                        (error/locatable operator "expected an expression after this")]
                       [else
                        (let-values ([(tokens rhs) (sub-parser tokens)])
                          (loop tokens (binary (locatable-line base)
                                               (locatable-character base)
                                               (punctuation-name operator)
                                               base
                                               rhs)))]))]
              [else
               (values tokens
                       base)]))))
  (define (parse/multiplication tokens)
    (parse/binary tokens MULTIPLICATION-OPERATORS parse/prefix))
  (define (parse/addition tokens)
    (parse/binary tokens ADDITION-OPERATORS parse/multiplication))
  (define (parse/comparison tokens)
    (parse/binary tokens COMPARISON-OPERATORS parse/addition))
  (define (parse/equality tokens)
    (parse/binary tokens EQUALITY-OPERATORS parse/comparison))
  (define (parse/logical tokens)
    (parse/binary tokens LOGICAL-OPERATORS parse/equality))
  
  (let-values ([(tokens logical) (parse/logical tokens)])
    (cond [(and (not (null? tokens)) (punctuation? (car tokens)) (string=? (punctuation-name (car tokens)) "?"))
           (let-values ([(tokens consequent) (parse/expression tokens line0)])
             (cond [(null? tokens)
                    (error/locatable consequent "expeted a colon after this")]
                   [(and (punctuation? (car tokens)) (string=? (punctuation-name (car tokens)) ":"))
                    (let ([colon (car tokens)]
                          [tokens (cdr tokens)])
                      (cond [(null? tokens)
                             (error/locatable colon "expected an expression after this")]
                            [else
                             (let-values ([(tokens alternative) (parse/expression tokens line0)])
                               (values tokens
                                       (ternary (locatable-line logical)
                                                (locatable-character logical)
                                                logical
                                                consequent
                                                alternative)))]))]
                   [else
                    (error/locatable (car tokens) "unexpected token")]))]
          [else
           (values tokens
                   logical)])))

;; (listof token) -> (listof token) ast
(define (parse/type tokens)
  (cond [(and (keyword? (car tokens)) (member (keyword-name (car tokens)) SIMPLE-TYPE-KEYWORDS))
         (values (cdr tokens)
                 (keyword-type (locatable-line (car tokens))
                               (locatable-character (car tokens))
                               (keyword-name (car tokens))))]
        [(and (keyword? (car tokens)) (string=? (keyword-name (car tokens)) "grade"))
         (let ([grade (car tokens)]
               [tokens (cdr tokens)])
           (cond [(null? tokens)
                  (error/locatable grade "expected an out-of operator after this")]
                 [(and (punctuation? (car tokens)) (string=? (punctuation-name (car tokens)) "//"))
                  (let ([out-of-operator (car tokens)]
                        [tokens (cdr tokens)])
                    (cond [(null? tokens)
                           (error/locatable out-of-operator "expected an expression after this")]
                          [else
                           (let-values ([(remains expression) (parse/expression tokens (locatable-line out-of-operator))])
                             (values remains
                                     (grade-type (locatable-line grade)
                                                 (locatable-character grade)
                                                 expression)))]))]
                 [else
                  (error/locatable (car tokens) "unexpected token")]))]
        [else
         (error/locatable (car tokens) "unexpected token")]))

;; (listof string) (listof number) -> (listof statement)
(define (parse-stmts lines line-numbers)
  (define (parse tokens)
    (define (parse/config tokens)
      (let ([keyword (car tokens)]
            [tokens (cdr tokens)])
        (cond [(null? tokens)
               (error/locatable keyword "expected a config option after this")]
              [(and (keyword? (car tokens)) (hash-has-key? CONFIG-OPTIONS (keyword-name (car tokens))))
               (let ([option (car tokens)]
                     [tokens (cdr tokens)])
                 (cond [(null? tokens)
                        (error/locatable option "expected an expression following this")]
                       [else
                        (let-values ([(remaining expression) (parse/expression tokens (locatable-line option))])
                          (cond [(null? remaining)
                                 (config-stmt (locatable-line keyword)
                                              (locatable-character keyword)
                                              (keyword-name option)
                                              expression)]
                                [else
                                 (error/locatable (car remaining) "unexpected token")]))]))]
              [else
               (error/locatable (car tokens) "expected a config option")])))
    (define (parse/input tokens)
      (define (finish keyword type prompt-expression tokens)
        (let ([to-keyword (car tokens)]
              [tokens (cdr tokens)])
          (cond [(null? tokens)
                 (error/locatable to-keyword "expected an identifier after this")]
                [(identifier? (car tokens))
                 (let ([identifier (car tokens)]
                       [tokens (cdr tokens)])
                   (cond [(null? tokens)
                          (input-stmt (locatable-line keyword)
                                      (locatable-character keyword)
                                      type
                                      prompt-expression
                                      (identifier-name identifier))]
                         [(and (keyword? (car tokens)) (string=? (keyword-name (car tokens)) "if"))
                          (let ([if-keyword (car tokens)]
                                [tokens (cdr tokens)])
                            (cond [(null? tokens)
                                   (error/locatable if-keyword "expected an expression after this")]
                                  [else
                                   (let-values ([(tokens predicate) (parse/expression tokens (locatable-line if-keyword))])
                                     (cond [(null? tokens)
                                            (error/locatable predicate "expected an 'else' clause after this")]
                                           [(and (keyword? (car tokens)) (string=? (keyword-name (car tokens)) "else"))
                                            (let ([else-keyword (car tokens)]
                                                  [tokens (cdr tokens)])
                                              (cond [(null? tokens)
                                                     (error/locatable else-keyword "expected an expression after this")]
                                                    [else
                                                     (let-values ([(tokens alternative) (parse/expression tokens (locatable-line else-keyword))])
                                                       (cond [(null? tokens)
                                                              (conditional-input-stmt (locatable-line keyword)
                                                                                      (locatable-character keyword)
                                                                                      type
                                                                                      prompt-expression
                                                                                      (identifier-name identifier)
                                                                                      predicate
                                                                                      alternative)]
                                                             [else
                                                              (error/locatable (car tokens) "unexpected token")]))]))]
                                           [else
                                            (error/locatable (car tokens) "unexpected token")]))]))]
                         [else
                          (error/locatable (car tokens) "unexpected token")]))]
                [else
                 (error/locatable (car tokens) "unexpected token")])))
      
      (let ([keyword (car tokens)]
            [tokens (cdr tokens)])
        (cond [(null? tokens)
               (error/locatable keyword "expected a type after this")]
              [else
               (let-values ([(tokens type) (parse/type tokens)])
                 (cond [(null? tokens)
                        (error/locatable type "expected a prompt or a target after this")]
                       [(and (keyword? (car tokens)) (string=? (keyword-name (car tokens)) "prompt"))
                        (let ([prompt-keyword (car tokens)]
                              [tokens (cdr tokens)])
                          (cond [(null? tokens)
                                 (error/locatable prompt-keyword "expected a prompt after this")]
                                [else
                                 (let-values ([(tokens prompt-expression) (parse/expression tokens (locatable-line prompt-keyword))])
                                   (cond [(null? tokens)
                                          (error/locatable prompt-expression "expected a target after this")]
                                         [else
                                          (cond [(and (keyword? (car tokens)) (string=? (keyword-name (car tokens)) "to"))
                                                 (finish keyword type prompt-expression tokens)]
                                                [else
                                                 (error/locatable (car tokens) "unexpected token")])]))]))]
                       [(and (keyword? (car tokens)) (string=? (keyword-name (car tokens)) "to"))
                        (finish keyword type #f tokens)]
                       [else
                        (error/locatable (car tokens) "unexpected token")]))])))
    (define (parse/let tokens)
      (let ([keyword (car tokens)]
            [tokens (cdr tokens)])
        (cond [(null? tokens)
               (error/locatable keyword "expected an identifier after this")]
              [(identifier? (car tokens))
               (let ([identifier (car tokens)]
                     [tokens (cdr tokens)])
                 (cond [(null? tokens)
                        (error/locatable keyword "expected an equals sign after this")]
                       [(and (punctuation? (car tokens)) (string=? (punctuation-name (car tokens)) "="))
                        (let ([equals-sign (car tokens)]
                              [tokens (cdr tokens)])
                          (cond [(null? tokens)
                                 (error/locatable equals-sign "expected an expression after this")]
                                [else
                                 (let-values ([(tokens expression) (parse/expression tokens (locatable-line equals-sign))])
                                   (cond [(null? tokens)
                                          (let-stmt (locatable-line keyword)
                                                    (locatable-character keyword)
                                                    (identifier-name identifier)
                                                    expression)]
                                         [else
                                          (error/locatable (car tokens) "unexpected token")]))]))]
                       [else
                        (error/locatable (car tokens) "unexpected token")]))]
              [else
               (error/locatable (car tokens) "unexpected token")])))
      
    (match tokens
      [(list) #f]
      [(list* (? (λ (t) (and (keyword? t) (string=? (keyword-name t) "config")))) _)
       (parse/config tokens)]
      [(list* (? (λ (t) (and (keyword? t) (string=? (keyword-name t) "input")))) _)
       (parse/input tokens)]
      [(list* (? (λ (t) (and (keyword? t) (string=? (keyword-name t) "let")))) _)
       (parse/let tokens)]
      [(list* t rest)
       (error/locatable t "unexpected token")]))
  
  (filter identity (map (λ (line line-num)
                          (parse (tokenize (substring line 1) line-num 2 tokenize/one/code)))
                        lines
                        line-numbers)))

;; (listof statement) -> (hash string value) (hash string any)
(define (evaluate-stmts stmts)
  (let loop ([env (hash)]
             [config CONFIG-OPTIONS]
             [stmts stmts])
    (values env config))) ; TODO: write this

;; value -> string
(define (pretty-print value)
  (define (string-escape str)
    (let loop ([str-chars (string->list str)]
               [out-chars '()])
      (cond [(null? str-chars) (list->string (reverse out-chars))]
            [else (case (car str-chars)
                    [(#\newline) (loop (cdr str-chars) (list* #\\ #\n out-chars))]
                    [(#\tab) (loop (cdr str-chars) (list* #\\ #\t out-chars))]
                    [(#\") (loop (cdr str-chars) (list* #\\ #\" out-chars))]
                    [(#\\) (loop (cdr str-chars) (list* #\\ #\\ out-chars))]
                    [else (loop (cdr str-chars) (cons (car str-chars) out-chars))])])))
  (cond [(number? value) (number->string value)]
        [(boolean? value) (if value "true" "false")]
        [(string? value) (string-append "\"" (string-escape value) "\"")]
        [(grade? value) (string-append (number->string (grade-value value))
                                       " out of "
                                       (number->string (grade-out-of value)))]))

;; (listof string) -> (listof string)
(define (body-lines-only lines)
  (let loop ([lines lines]
             [line-nums (build-list (length lines) add1)])
    (cond [(null? lines) (values '() '())]
          [else (if (or (string-prefix? (car lines) "$")
                        (blank-line? (car lines)))
                    (loop (cdr lines) (cdr line-nums))
                    (values lines line-nums))])))

;; (listof string) (listof number) -> (listof line)
(define (parse-bodies lines line-numbers)
  ;; (listof token) -> line
  (define (parse tokens)
    (let loop ([tokens tokens]
               [rsf '()])
      (cond [(null? tokens)
             (line 0 0 (reverse rsf))]
            [else (cond [(plain-text? (car tokens))
                         (loop (cdr tokens) (cons (car tokens) rsf))]
                        [else
                         (let-values ([(tokens ast) (parse/expression tokens (locatable-line (car tokens)))])
                           (loop tokens (cons ast rsf)))])])))
  
  (map (λ (line line-num)
         (parse (tokenize line line-num 1 tokenize/one/body)))
       lines
       line-numbers))

;; (listof line) -> (listof string)
(define (evaluate-lines lines env config)
  '()) ; TODO: write this

;; (listof string) -> (listof string)
(define (collapse-newlines lines)
  (define (collapse-newlines/no-nl lines)
    (cond [(null? lines) null]
          [else (if (blank-line? (car lines))
                    (cons "" (collapse-newlines/munch-nl (cdr lines)))
                    (cons (car lines) (collapse-newlines/no-nl (cdr lines))))]))
  (define (collapse-newlines/munch-nl lines)
    (cond [(null? lines) null]
          [else (if (blank-line? (car lines))
                    (collapse-newlines/munch-nl (cdr lines))
                    (cons (car lines) (collapse-newlines/no-nl (cdr lines))))]))
  (let loop ([reversed-lines (reverse (collapse-newlines/munch-nl lines))])
    (cond [(null? reversed-lines) null]
          [else (if (blank-line? (car reversed-lines))
                    (loop (cdr reversed-lines))
                    (reverse reversed-lines))])))

(define (render-rml lines output-port)
  (let*-values ([(stmt-lines stmt-line-nums) (stmt-lines-only lines)]
                [(env config) (evaluate-stmts (parse-stmts stmt-lines stmt-line-nums))]
                [(body-lines body-line-nums) (body-lines-only lines)]
                [(output-lines) ((if (hash-ref config "newline_collapse")
                                     collapse-newlines
                                     identity)
                                 (evaluate-lines (parse-bodies body-lines body-line-nums) env config))])
    (for-each (λ (line) (displayln line output-port)) output-lines)
    (when (debug)
      (displayln "debug: Environment" (current-error-port))
      (hash-for-each env
                     (λ (key value)
                       (displayln (format "debug: ~a = ~a"
                                          key
                                          (pretty-print value))
                                  (current-error-port))))
      (displayln "debug: Config Values" (current-error-port))
      (hash-for-each config
                     (λ (key value)
                       (displayln (format "debug: ~a = ~a"
                                          key
                                          (pretty-print value))
                                  (current-error-port)))))))

(command-line
 #:once-each
 [("-o") out-filename*
         "File to output rendered RML to"
         (out-filename out-filename*)]
 [("--debug") ("Set debug mode"
               "Prints out the value of the environment and config before rendering")
              (debug #t)]
 #:args ([in-filename* "example.rml"])
 (in-filename in-filename*)
 (with-handlers ([exn:fail:filesystem?
                  (λ (e) (displayln (format "Could not open file '~a' for reading"
                                            in-filename)
                                    (current-error-port)))])
   (let ([lines (file->lines (in-filename))])
     (if (null? (out-filename))
         (render-rml lines (current-output-port))
         (with-handlers ([exn:fail:filesystem?
                          (λ (e) (displayln (format "Could not open file '~a' for writing"
                                                    (out-filename))))])
           (call-with-output-file* (out-filename)
                                   (λ (output-port) (render-rml lines output-port))
                                   #:mode 'text
                                   #:exists 'truncate/replace))))))