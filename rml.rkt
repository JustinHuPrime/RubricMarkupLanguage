#!/usr/bin/env racket
#lang racket

(require racket/cmdline)

(define (render-rml)
  (void))

(let ([out-filename (make-parameter null)])
  (command-line
   #:once-each
   [("-o") out-filename*
           "File to output rendered RML to"
           (out-filename out-filename*)]
   #:args (in-filename)
   (with-handlers ([exn:fail:filesystem?
                    (λ (e) (displayln (format "Could not open file '~a' for reading"
                                              in-filename)
                                      (current-error-port)))])
     (with-input-from-file in-filename
       (λ () (if (null? (out-filename))
                 (render-rml)
                 (with-handlers ([exn:fail:filesystem?
                                  (λ (e) (displayln (format "Could not open file '~a' for writing"
                                                            (out-filename))))])
                   (with-output-to-file (out-filename)
                     render-rml
                     #:mode 'text
                     #:exists 'truncate/replace))))
       #:mode 'text))))