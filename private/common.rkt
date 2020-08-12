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

(provide out-filename)
(define out-filename (make-parameter null))
(provide in-filename)
(define in-filename (make-parameter null))
(provide debug)
(define debug (make-parameter #f))