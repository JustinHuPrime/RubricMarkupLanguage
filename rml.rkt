#!/usr/bin/env racket
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

(require racket/cmdline)
(require "private/ast.rkt")
(require "private/parser.rkt")
(require "private/utils.rkt")
(require "private/common.rkt")

;; expression (hash string any) -> any
(define (evaluate-expr expr env config)
  (cond [(identifier? expr)
         (hash-ref env
                   (identifier-name expr)
                   (thunk (error/locatable expr "undefined identifier")))]
        [(literal? expr)
         (literal-value expr)]
        [(ternary? expr)
         (let ([predicate (evaluate-expr (ternary-predicate expr)
                                         env
                                         config)])
           (cond [(not (boolean? predicate))
                  (error/locatable expr "ternary expression expects a boolean predicate")]
                 [predicate
                  (evaluate-expr (ternary-consequent expr)
                                 env
                                 config)]
                 [else
                  (evaluate-expr (ternary-alternative expr)
                                 env
                                 config)]))]
        [(binary? expr)
         (case (binary-operator expr)
           [("||")
            (let ([lhs (evaluate-expr (binary-lhs expr)
                                      env
                                      config)])
              (cond [(not (boolean? lhs))
                     (error/locatable (binary-lhs expr) "or expression expects a boolean on the left hand side")]
                    [lhs
                     #t]
                    [else
                     (let ([rhs (evaluate-expr (binary-rhs expr)
                                               env
                                               config)])
                       (if (not (boolean? rhs))
                           (error/locatable (binary-rhs expr) "or expression expects a boolean on the right hand side")
                           rhs))]))]
           [("&&")
            (let ([lhs (evaluate-expr (binary-lhs expr)
                                      env
                                      config)])
              (cond [(not (boolean? lhs))
                     (error/locatable (binary-lhs expr) "and expression expects a boolean on the left hand side")]
                    [(not lhs)
                     #f]
                    [else
                     (let ([rhs (evaluate-expr (binary-rhs expr)
                                               env
                                               config)])
                       (if (not (boolean? rhs))
                           (error/locatable (binary-rhs expr) "and expression expects a boolean on the right hand side")
                           rhs))]))]
           [("==" "!=")
            (let ([lhs (evaluate-expr (binary-lhs expr)
                                      env
                                      config)]
                  [rhs (evaluate-expr (binary-rhs expr)
                                      env
                                      config)])
              (cond [(not (type=? lhs rhs))
                     (error/locatable expr "cannot compare two unlike types")]
                    [(boolean? lhs)
                     (case (binary-operator expr)
                       [("==") (boolean=? lhs rhs)]
                       [("!=") (not (boolean=? lhs rhs))])]
                    [(number? lhs)
                     (case (binary-operator expr)
                       [("==") (= lhs rhs)]
                       [("!=") (not (= lhs rhs))])]
                    [(string? lhs)
                     (case (binary-operator expr)
                       [("==") (string=? lhs rhs)]
                       [("==") (not (string=? lhs rhs))])]
                    [else
                     (if (not (= (grade-out-of lhs) (grade-out-of rhs)))
                         (error/locatable expr "cannot compare grades out of different values")
                         (case (binary-operator expr)
                           [("==") (= (grade-value lhs) (grade-value rhs))]
                           [("!=") (not (= (grade-value lhs) (grade-value rhs)))]))]))]
           [("<" "<=" ">" ">=")
            (let ([lhs (evaluate-expr (binary-lhs expr)
                                      env
                                      config)]
                  [rhs (evaluate-expr (binary-rhs expr)
                                      env
                                      config)])
              (cond [(not (type=? lhs rhs))
                     (error/locatable expr "cannot compare two unlike types")]
                    [(or (string? lhs) (boolean? lhs))
                     (error/locatable expr "cannot compare non-numeric types")]
                    [(number? lhs)
                     (case (binary-operator expr)
                       [("<") (< lhs rhs)]
                       [("<=") (<= lhs rhs)]
                       [(">") (> lhs rhs)]
                       [(">=") (>= lhs rhs)])]
                    [else
                     (if (not (= (grade-out-of lhs) (grade-out-of rhs)))
                         (error/locatable expr "cannot compare grades out of different values")
                         (case (binary-operator expr)
                           [("<") (< (grade-value lhs) (grade-value rhs))]
                           [("<=") (<= (grade-value lhs) (grade-value rhs))]
                           [(">") (> (grade-value lhs) (grade-value rhs))]
                           [(">=") (>= (grade-value lhs) (grade-value rhs))]))]))]
           [("+")
            (let ([lhs (evaluate-expr (binary-lhs expr)
                                      env
                                      config)]
                  [rhs (evaluate-expr (binary-rhs expr)
                                      env
                                      config)])
              (cond [(or (and (grade? lhs) (number? rhs))
                         (and (grade? rhs) (number? lhs)))
                     (let ([grade-val (if (grade? lhs) lhs rhs)]
                           [number-val (if (number? lhs) lhs rhs)])
                       (cond [(> (+ (grade-value grade-val) number-val) (grade-out-of grade-val))
                              (error/locatable expr "addition would put grade over maximum grade")]
                             [else
                              (grade (+ (grade-value grade-val) number-val)
                                     (grade-out-of grade-val))]))]
                    [(not (type=? lhs rhs))
                     (error/locatable expr "cannot add two unlike types (except for grades and numbers)")]
                    [(boolean? lhs)
                     (error/locatable expr "cannot add booleans")]
                    [(number? lhs)
                     (+ lhs rhs)]
                    [(string? lhs)
                     (string-append lhs rhs)]
                    [else
                     (grade (+ (grade-value lhs) (grade-value rhs))
                            (+ (grade-out-of lhs) (grade-out-of rhs)))]))]
           [("-")
            (let ([lhs (evaluate-expr (binary-lhs expr)
                                      env
                                      config)]
                  [rhs (evaluate-expr (binary-rhs expr)
                                      env
                                      config)])
              (cond [(and (grade? lhs) (number? rhs))
                     (cond [(< (- (grade-value lhs) rhs) 0)
                            (error/locatable expr "subtraction would put grade under zero")]
                           [else
                            (grade (- (grade-value lhs) rhs)
                                   (grade-out-of lhs))])]
                    [(not (type=? lhs rhs))
                     (error/locatable expr "cannot subtract two unlike types (except a number from a grade)")]
                    [(boolean? lhs)
                     (error/locatable expr "cannot subtract two booleans")]
                    [(number? lhs)
                     (- lhs rhs)]
                    [(string? lhs)
                     (error/locatable expr "cannot subtract two strings")]
                    [else
                     (error/locatable expr "cannot subtract two grades")]))]
           [("*" "/" "%" "//")
            (let ([lhs (evaluate-expr (binary-lhs expr)
                                      env
                                      config)]
                  [rhs (evaluate-expr (binary-rhs expr)
                                      env
                                      config)])
              (cond [(not (type=? lhs rhs))
                     (error/locatable expr (case (binary-operator expr)
                                             [("*") "cannot multiply two unlike types"]
                                             [("/") "cannot divide two unlike types"]
                                             [("%") "cannot modulo two unlike types"]
                                             [("//") "cannot form a grade from two unlike types"]))]
                    [(not (number? lhs))
                     (error/locatable expr (case (binary-operator expr)
                                             [("*") "cannot multiply non-numbers"]
                                             [("/") "cannot divide non-numbers"]
                                             [("%") "cannot modulo non-numbers"]
                                             [("//") "cannot form a grade from non-numbers"]))]
                    [else
                     (case (binary-operator expr)
                       [("*") (* lhs rhs)]
                       [("/") (if (zero? rhs)
                                  (error/locatable rhs "attempted division by zero")
                                  (/ lhs rhs))]
                       [("%") (if (zero? rhs)
                                  (error/locatable rhs "attempted modulo by zero")
                                  (remainder lhs rhs))]
                       [("//") (if (zero? rhs)
                                   (error/locatable rhs "attempted to form a grade out of zero")
                                   (grade lhs rhs))])]))])]
        [(unary? expr)
         (let ([target (evaluate-expr (unary-target expr)
                                      env
                                      config)])
           (case (unary-operator expr)
             [("-")
              (if (not (number? target))
                  (error/locatable expr "cannot negate a non-number")
                  (- target))]
             [("!")
              (if (not (boolean? target))
                  (error/locatable expr "cannot perform logical not on a non-boolean")
                  (not target))]
             [("floor" "ceil" "round")
              (if (not (number? target))
                  (error/locatable expr (case (unary-operator expr)
                                          [("floor") "cannot perform floor on a non-number"]
                                          [("ceil") "cannot perform ceiling on a non-number"]
                                          [("round") "cannot round a non-number"]))
                  (case (unary-operator expr)
                    [("floor") (floor target)]
                    [("ceil") (ceiling target)]
                    [("round") (round target)]))]
             [("to_string")
              (cond [(boolean? target) (if target
                                           (hash-ref config "print_true_string")
                                           (hash-ref config "print_false_string"))]
                    [(number? target) (number->string/limited target
                                                              (hash-ref config "number_print_digits"))]
                    [(string? target) target]
                    [else (string-append (number->string/limited (grade-value target)
                                                                 (hash-ref config "number_print_digits"))
                                         (hash-ref config "print_out_of_string")
                                         (number->string/limited (grade-out-of target)
                                                                 (hash-ref config "number_print_digits")))])]
             [("to_number")
              (cond [(boolean? target) (if target 1 0)]
                    [(number? target) target]
                    [(string? target) (let ([number (string->number target 10 'number-or-false 'decimal-as-exact)])
                                        (if number
                                            number
                                            (error/locatable expr "attempted to convert non-numeric string to number")))]
                    [else (/ (grade-value target) (grade-out-of target))])]
             [("out_of" "score")
              (if (not (grade? target))
                  (error/locatable expr "cannot query grade components on non-grade")
                  (case (unary-operator expr)
                    [("out_of") (grade-out-of target)]
                    [("score") (grade-value target)]))]))]))

;; string type hash -> any
(define (handle-input prompt type config)
  (display prompt)
  (let ([input (read-line (current-input-port) 'any)])
    (cond [(and (keyword-type? type) (string=? (keyword-type-name type) "boolean"))
           (cond [(or (string-ci=? input "true") (string-ci=? input "t"))
                  #t]
                 [(or (string-ci=? input "false") (string-ci=? input "f"))
                  #f]
                 [else
                  (error/input "~a is not a boolean" (pretty-print input config))])]
          [(and (keyword-type? type) (string=? (keyword-type-name type) "number"))
           (let ([value (string->number input 10 'number-or-false 'decimal-as-exact)])
             (if value
                 value
                 (error/input "~a is not a number" (pretty-print input config))))]
          [(and (keyword-type? type) (string=? (keyword-type-name type) "string"))
           input]
          [else
           (let ([value (string->number input 10 'number-or-false 'decimal-as-exact)])
             (cond [(not value)
                    (error/input "~a is not a number" (pretty-print input config))]
                   [(not (<= 0 value (grade-type-out-of type)))
                    (error/input "~a must be between ~a and 0" value (grade-type-out-of type))]
                   [else
                    (grade value (grade-type-out-of type))]))])))

;; (listof statement) -> (hash string any) (hash string any)
(define (evaluate-stmts stmts)
  (let loop ([env (hash)]
             [config CONFIG-OPTIONS]
             [stmts stmts])
    (cond [(null? stmts)
           (values env config)]
          [else (cond [(config-stmt? (car stmts))
                       (loop env
                             (hash-set config
                                       (config-stmt-name (car stmts))
                                       (let ([value (evaluate-expr (config-stmt-expr (car stmts))
                                                                   env
                                                                   config)])
                                         (case (config-stmt-name (car stmts))
                                           [("newline_collapse")
                                            (if (not (boolean? value))
                                                (error/locatable (config-stmt-expr (car stmts))
                                                                 "~a expects a boolean"
                                                                 (config-stmt-name (car stmts)))
                                                value)]
                                           [("number_print_digits")
                                            (if (not (and (integer? value) (positive? value)))
                                                (error/locatable (config-stmt-expr (car stmts))
                                                                 "~a expects a positive integer"
                                                                 (config-stmt-name (car stmts)))
                                                value)]
                                           [("print_true_string" "print_false_string" "print_out_of_string")
                                            (if (not (string? value))
                                                (error/locatable (config-stmt-expr (car stmts))
                                                                 "~a expects a string"
                                                                 (config-stmt-name (car stmts)))
                                                value)])))
                             (cdr stmts))]
                      [(input-stmt? (car stmts))
                       (cond [(hash-has-key? env (input-stmt-name (car stmts)))
                              (error/locatable (car stmts) "redefinition of existing value")]
                             [else
                              (let* ([type (cond [(keyword-type? (input-stmt-input-type (car stmts)))
                                                  (input-stmt-input-type (car stmts))]
                                                 [else
                                                  (grade-type (locatable-line (input-stmt-input-type (car stmts)))
                                                              (locatable-character (input-stmt-input-type (car stmts)))
                                                              (let ([out-of (evaluate-expr (grade-type-out-of (input-stmt-input-type (car stmts)))
                                                                                           env
                                                                                           config)])
                                                                (if (not (and (real? out-of) (positive? out-of)))
                                                                    (error/locatable (input-stmt-input-type (car stmts)) "grade out-of is not a positive number")
                                                                    out-of)))])]
                                     [prompt (if (input-stmt-prompt (car stmts))
                                                 (let ([prompt (evaluate-expr (input-stmt-prompt (car stmts))
                                                                              env
                                                                              config)])
                                                   (if (not (string? prompt))
                                                       (error/locatable (input-stmt-prompt (car stmts)) "prompt is not a string")
                                                       prompt))
                                                 (format "~a (~a): "
                                                         (input-stmt-name (car stmts))
                                                         (if (grade-type? type)
                                                             (format "grade out of ~a"
                                                                     (pretty-print (grade-type-out-of type) config))
                                                             (keyword-type-name type))))])
                                (let ([input-value (handle-input prompt type config)])
                                  (loop (hash-set env (input-stmt-name (car stmts)) input-value)
                                        config
                                        (cdr stmts))))])]
                      [(conditional-input-stmt? (car stmts))
                       (cond [(hash-has-key? env (conditional-input-stmt-name (car stmts)))
                              (error/locatable (car stmts) "redefinition of existing value")]
                             [else
                              (let* ([predicate (evaluate-expr (conditional-input-stmt-predicate (car stmts))
                                                               env
                                                               config)])
                                (cond [(not (boolean? predicate))
                                       (error/locatable (conditional-input-stmt-predicate (car stmts)) "conditional input statement expects a boolean predicate")]
                                      [predicate
                                       (let* ([type (cond [(keyword-type? (conditional-input-stmt-input-type (car stmts)))
                                                           (conditional-input-stmt-input-type (car stmts))]
                                                          [else
                                                           (grade-type (locatable-line (conditional-input-stmt-input-type (car stmts)))
                                                                       (locatable-character (conditional-input-stmt-input-type (car stmts)))
                                                                       (let ([out-of (evaluate-expr (grade-type-out-of (conditional-input-stmt-input-type (car stmts)))
                                                                                                    env
                                                                                                    config)])
                                                                         (if (not (and (real? out-of) (positive? out-of)))
                                                                             (error/locatable (conditional-input-stmt-input-type (car stmts)) "grade out-of is not a positive number")
                                                                             out-of)))])]
                                              [prompt (if (conditional-input-stmt-prompt (car stmts))
                                                          (let ([prompt (evaluate-expr (conditional-input-stmt-prompt (car stmts))
                                                                                       env
                                                                                       config)])
                                                            (if (not (string? prompt))
                                                                (error/locatable (conditional-input-stmt-prompt (car stmts)) "prompt is not a string")
                                                                prompt))
                                                          (format "~a (~a): "
                                                                  (conditional-input-stmt-name (car stmts))
                                                                  (if (grade-type? type)
                                                                      (format "grade out of ~a"
                                                                              (pretty-print (grade-type-out-of type) config))
                                                                      (keyword-type-name type))))])
                                         (let ([input-value (handle-input prompt type config)])
                                           (loop (hash-set env (conditional-input-stmt-name (car stmts)) input-value)
                                                 config
                                                 (cdr stmts))))]
                                      [else
                                       (loop (hash-set env
                                                       (conditional-input-stmt-name (car stmts))
                                                       (evaluate-expr (conditional-input-stmt-alternative (car stmts))
                                                                      env
                                                                      config))
                                             config
                                             (cdr stmts))]))])]
                      [(let-stmt? (car stmts))
                       (cond [(hash-has-key? env (let-stmt-name (car stmts)))
                              (error/locatable (car stmts) "redefinition of existing value")]
                             [else
                              (loop (hash-set env
                                              (let-stmt-name (car stmts))
                                              (evaluate-expr (let-stmt-value (car stmts))
                                                             env
                                                             config))
                                    config
                                    (cdr stmts))])])])))

;; (listof line) -> (listof string)
(define (evaluate-lines lines env config)
  (let loop ([lines lines]
             [rsf '()])
    (cond [(null? lines)
           (reverse rsf)]
          [else
           (loop (cdr lines)
                 (cons (let loop ([exprs (line-exprs (car lines))]
                                  [rsf '()])
                         (cond [(null? exprs)
                                (foldl string-append "" rsf)]
                               [else
                                (cond [(plain-text? (car exprs))
                                       (loop (cdr exprs)
                                             (cons (plain-text-text (car exprs)) rsf))]
                                      [else
                                       (let ([result (evaluate-expr (car exprs) env config)])
                                         (cond [(number? result)
                                                (loop (cdr exprs)
                                                      (cons (number->string/limited result (hash-ref config "number_print_digits")) rsf))]
                                               [(boolean? result)
                                                (loop (cdr exprs)
                                                      (cons (if result
                                                                (hash-ref config "print_true_string")
                                                                (hash-ref config "print_false_string"))
                                                            rsf))]
                                               [(string? result)
                                                (loop (cdr exprs)
                                                      (cons result rsf))]
                                               [else
                                                (loop (cdr exprs)
                                                      (cons (string-append (number->string/limited (grade-value result) (hash-ref config "number_print_digits"))
                                                                           (hash-ref config "print_out_of_string")
                                                                           (number->string/limited (grade-out-of result) (hash-ref config "number_print_digits")))
                                                            rsf))]))])]))
                       rsf))])))

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
                                          (pretty-print value config))
                                  (current-error-port))))
      (displayln "debug: Config Values" (current-error-port))
      (hash-for-each config
                     (λ (key value)
                       (displayln (format "debug: ~a = ~a"
                                          key
                                          (pretty-print value config))
                                  (current-error-port)))))))

(command-line
 #:usage-help
 "Rubric Markup Language (RML) version 0.1.0"
 "Copyright 2020 Justin Hu"
 "This is free software; see the source for copying conditions. There is NO"
 "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
 #:once-each
 [("-o") out-filename*
         "File to output rendered RML to"
         (out-filename out-filename*)]
 [("--debug") ("Set debug mode"
               "Prints out the value of the environment and config before rendering")
              (debug #t)]
 #:args (in-filename*)
 (in-filename in-filename*)
 (with-handlers ([exn:fail:filesystem?
                  (λ (e) (displayln (format "Could not open file '~a' for reading"
                                            (in-filename))
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
