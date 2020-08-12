#lang racket

;; values are either Numbers, Booleans, Strings, or Grades
(provide (struct-out grade))
(struct grade (value out-of) #:transparent)

(provide locatable? locatable-line locatable-character)
(struct locatable (line character) #:transparent)

(provide token?)
(struct token locatable () #:transparent)
;; string
(provide (struct-out plain-text))
(struct plain-text token (text) #:transparent)
(provide KEYWORDS)
(define KEYWORDS '("config"
                   "newline_collapse"
                   "number_print_digits"
                   "print_true_string"
                   "print_false_string"
                   "print_out_of_string"
                   "input"
                   "prompt"
                   "to"
                   "if"
                   "else"
                   "let"
                   "floor"
                   "ceil"
                   "round"
                   "to_string"
                   "to_number"
                   "out_of"
                   "score"
                   "boolean"
                   "number"
                   "string"
                   "grade"
                   "true"
                   "false"))
(provide CONFIG-OPTIONS)
(define CONFIG-OPTIONS (hash "newline_collapse" #t
                             "number_print_digits" 2
                             "print_true_string" "true"
                             "print_false_string" "false"
                             "print_out_of_string" " out of "))
(provide (struct-out keyword))
(struct keyword token (name) #:transparent)
(provide (struct-out punctuation))
(struct punctuation token (name) #:transparent)
(provide (struct-out identifier))
(struct identifier token (name) #:transparent)
(provide (struct-out literal))
(struct literal token (value) #:transparent)

(provide locatable?)
(struct ast locatable () #:transparent)
;; string, expression
(provide (struct-out config-stmt))
(struct config-stmt ast (name expr) #:transparent)
;; type (or/c expression #f) string
(provide (struct-out input-stmt))
(struct input-stmt ast (input-type prompt name) #:transparent)
;; type, expression, string, expression, expression
(provide (struct-out conditional-input-stmt))
(struct conditional-input-stmt ast (input-type prompt name predicate alternative) #:transparent)
;; string, expression
(provide (struct-out let-stmt))
(struct let-stmt ast (name value) #:transparent)

;; (listof expression plain-text)
(provide (struct-out line))
(struct line ast (exprs) #:transparent)
;; expression expression expression
(provide (struct-out ternary))
(struct ternary ast (predicate consequent alternative) #:transparent)
(provide LOGICAL-OPERATORS)
(define LOGICAL-OPERATORS '("&&"
                            "||"))
(provide EQUALITY-OPERATORS)
(define EQUALITY-OPERATORS '("=="
                             "!="))
(provide COMPARISON-OPERATORS)
(define COMPARISON-OPERATORS '("<"
                               "<="
                               ">"
                               ">="))
(provide ADDITION-OPERATORS)
(define ADDITION-OPERATORS '("+"
                             "-"))
(provide MULTIPLICATION-OPERATORS)
(define MULTIPLICATION-OPERATORS '("*"
                                   "/"
                                   "%"
                                   "//"))
;; string expression expression
(provide (struct-out binary))
(struct binary ast (operator lhs rhs) #:transparent)
(provide PREFIX-OPERATORS)
(define PREFIX-OPERATORS '("-"
                           "!"))
(provide PRIMARY-OPERATORS)
(define PRIMARY-OPERATORS '("floor"
                            "ceil"
                            "round"
                            "to_string"
                            "to_number"
                            "out_of"
                            "score"))
;; string expression
(provide (struct-out unary))
(struct unary ast (operator target) #:transparent)

(provide SIMPLE-TYPE-KEYWORDS)
(define SIMPLE-TYPE-KEYWORDS '("boolean"
                               "number"
                               "string"))
;; string
(provide (struct-out keyword-type))
(struct keyword-type ast (name) #:transparent)
;; expression
(provide (struct-out grade-type))
(struct grade-type ast (out-of) #:transparent)