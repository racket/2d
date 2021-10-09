#lang info

(define collection "2d")
(define version "1.0")
(define deps '("base" "2d-lib" "racket-index"))
(define build-deps '("rackunit-lib"
                     "option-contract-lib"
                     "at-exp-lib"
                     "gui-lib"
                     "syntax-color-lib"))
(define pkg-desc "tests for \"2d\"")
(define pkg-authors '(robby))

(define license
  '(Apache-2.0 OR MIT))
