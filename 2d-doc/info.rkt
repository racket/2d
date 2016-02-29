#lang info

(define collection "2d")
(define version "1.0")
(define deps '("base" "2d-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define pkg-desc "Documentation part of \"2d\"")
(define pkg-authors '(robby))

(define scribblings '(("scribblings/2d.scrbl" () ("Syntax Extensions"))))
