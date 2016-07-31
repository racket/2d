#lang racket/base

(require "private/readtable.rkt"
         racket/contract)
(provide
 (contract-out
  [2d-readtable-dispatch-proc
   (-> char? input-port? any/c
       (or/c exact-positive-integer? #f)
       (or/c exact-nonnegative-integer? #f)
       (or/c exact-positive-integer? #f)
       (-> input-port? any/c (or/c readtable? #f) any/c)
       (or/c #f readtable?)
       any/c)])
 make-readtable)
