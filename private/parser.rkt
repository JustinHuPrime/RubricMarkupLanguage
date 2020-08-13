#lang racket

;; Copyright 2020 Justin Hu
;;
;; This file is part of the Rubric Markup Language
;;
;; The Rubric Markup Language is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; The Rubric Markup Language is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; the Rubric Markup Language. If not see <https://www.gnu.org/licenses/>.
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(require "ast.rkt")
(require "utils.rkt")

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
                    [(list* #\$ other rest) (error/nums line-num
                                                        (+ char-num length-adjustment (length rsf))
                                                        "expected either $$ or ${")]
                    [(list* c rest) (loop rest (cons c rsf) length-adjustment)])])))
  
  (cond [(null? chars) (values '() char-num #f)]
        [else (match chars
                [(list* #\$ #\{ rest) (let ([code-length (index-of rest #\})])
                                        (if code-length
                                            (let ([code-string (list->string (take rest code-length))]
                                                  [remainder (drop rest (+ code-length 1))])
                                              (values remainder
                                                      (+ char-num 3 code-length)
                                                      (reverse (tokenize code-string line-num (+ char-num 2) tokenize/one/code))))
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
(provide stmt-lines-only)
(define (stmt-lines-only lines)
  (let loop ([rsf-lines '()]
             [rsf-line-nums '()]
             [lines lines]
             [line-nums (build-list (length lines) add1)])
    (cond [(null? lines) (values (reverse rsf-lines)
                                 (reverse rsf-line-nums))]
          [else (cond [(blank-line? (car lines))
                       (loop rsf-lines rsf-line-nums (cdr lines) (cdr line-nums))]
                      [(string-prefix? (car lines) "$#")
                       (loop rsf-lines rsf-line-nums (cdr lines) (cdr line-nums))]
                      [(and (string-prefix? (car lines) "$")
                            (not (string-prefix? (car lines) "$$")))
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
           (let-values ([(tokens expression) (parse/expression (cdr tokens) line0)])
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
                    (let-values ([(tokens target) (parse/expression (cdr tokens) line0)])
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
    (cond [(and (not (null? tokens)) (punctuation? (car tokens)) (member (punctuation-name (car tokens)) PREFIX-OPERATORS))
           (let ([operator (car tokens)]
                 [tokens (cdr tokens)])
             (cond [(null? tokens)
                    (error/locatable operator "expected an expresison after this")]
                   [else
                    (let-values ([(remainder target) (parse/prefix tokens)])
                      (values remainder
                              (unary (locatable-line operator)
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
           (let-values ([(tokens consequent) (parse/expression (cdr tokens) line0)])
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
(provide parse-stmts)
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

;; (listof string) -> (listof string)
(provide body-lines-only)
(define (body-lines-only lines)
  (let loop ([lines lines]
             [line-nums (build-list (length lines) add1)])
    (cond [(null? lines) (values '() '())]
          [else (if (or (and (string-prefix? (car lines) "$")
                             (not (string-prefix? (car lines) "$$")))
                        (blank-line? (car lines)))
                    (loop (cdr lines) (cdr line-nums))
                    (values lines line-nums))])))

;; (listof string) (listof number) -> (listof line)
(provide parse-bodies)
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
