#lang racket

(provide out-filename)
(define out-filename (make-parameter null))
(provide in-filename)
(define in-filename (make-parameter null))
(provide debug)
(define debug (make-parameter #f))