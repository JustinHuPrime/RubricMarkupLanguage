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

(require "common.rkt")
(require "ast.rkt")

(provide blank-line?)
(define (blank-line? line)
  (andmap char-whitespace? (string->list line)))

(provide error/nums)
(define (error/nums line character format-string . format-args)
  (raise (exn:fail:user (apply format
                               (string-append "~a:~a:~a: " format-string)
                               (in-filename)
                               line
                               character
                               format-args)
                        (current-continuation-marks))))
(provide error/locatable)
(define (error/locatable locatable format-string . format-args)
  (apply error/nums
         (locatable-line locatable)
         (locatable-character locatable)
         format-string
         format-args))

(provide error/input)
(define (error/input format-string . format-args)
  (raise (exn:fail:user (apply format
                               (string-append "input: " format-string)
                               format-args)
                        (current-continuation-marks))))

(provide number->string/limited)
(define (number->string/limited n digits)
  (let ([decimal-string (number->string (exact->inexact (/ (round (* n (expt 10 digits)))
                                                           (expt 10 digits))))])
    (if (string-suffix? decimal-string ".0")
        (substring decimal-string 0 (- (string-length decimal-string) 2))
        decimal-string)))

;; value hash -> string
(provide pretty-print)
(define (pretty-print value config)
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
  (cond [(number? value) (number->string/limited value (hash-ref config "number_print_digits"))]
        [(boolean? value) (if value (hash-ref config "print_true_string") (hash-ref config "print_false_string"))]
        [(string? value) (string-append "\"" (string-escape value) "\"")]
        [else (string-append (number->string/limited (grade-value value) (hash-ref config "number_print_digits"))
                             (hash-ref config "print_out_of_string")
                             (number->string/limited (grade-out-of value) (hash-ref config "number_print_digits")))]))

;; (listof string) -> (listof string)
(provide collapse-newlines)
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

;; any any -> boolean
(provide type=?)
(define (type=? a b)
  (cond [(boolean? a)
         (boolean? b)]
        [(number? a)
         (number? b)]
        [(string? a)
         (string? b)]
        [else
         (grade? b)]))
        
