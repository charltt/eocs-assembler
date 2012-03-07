#lang racket
(require "asm-base.rkt"
         "asm-support.rkt")

(provide parse
         ;; Export parse-one-line
         ;; so we can test it.
         parse-one-line)

;; CONTRACT
;; parse :: list-of strings -> list-of ASM structures
;; PURPOSE
;; Takes in a list of strings and returns a list
;; of ASM structures (and a symbol or two).
(define (parse los)
  (cond
    [(empty? los) '()]
    [else
     (cons (parse-one-line (first los))
           (parse (rest los)))]))

;; CONTRACT
;; parse-one-line :: string -> (U A? C? label? symbol?)
;; PURPOSE
;; Parses a string representing a single assembly
;; language instruction.
(define (parse-one-line asm)
  (match asm
    [(regexp "^\\s*$") 'blank-line]
    [(regexp DCJ-REGEXP)
     (C 'no-line (extract-dest asm) (extract-comp asm) (extract-jump asm))]
    [(regexp CJ-REGEXP)
     (C 'no-line 'no-dest (extract-comp asm) (extract-jump asm))]
    [(regexp DC-REGEXP)
     (C 'no-line (extract-dest asm) (extract-comp asm) 'no-jump)]
    [(regexp C-REGEXP)
     (C 'no-line 'no-dest (extract-comp asm) 'no-jump)]
    [(regexp ANUM-REGEXP)
     (A 'no-line (@inst->number asm))]
    [(regexp ASYM-REGEXP)
     (A 'no-line (@inst->symbol asm))]
    [(regexp LABEL-REGEXP)
     (label 'no-line (string->symbol (second (regexp-match LABEL-REGEXP asm))))]
    ;; If all else fails, emit a parse bogon.
    [else 'parse-bogon]
    ))