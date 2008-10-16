;;;; ccs-parser-test.lisp

(defpackage #:ritucci-msc-ccs-parser-test
  (:use #:common-lisp #:ritucci-msc-ccs-parser #:lisp-unit))


(in-package #:ritucci-msc-ccs-parser-test)


(define-test is-action
	     (assert-true (is-action "a"))
	     (assert-true (is-action "prova"))
	     (assert-true (is-action "&"))	; tau
	     (assert-false (is-action "Prova"))
	     (assert-false (is-action "PROVA")))


(define-test is-label
	     (assert-false (is-label "&"))
	     (assert-true (is-action "prova"))
	     (assert-true (is-action "&"))	; tau
	     (assert-false (is-action "Prova"))
	     (assert-false (is-action "PROVA")))

(define-test is-process
	     (assert-true (is-process "Ciao"))
	     (assert-true (is-process "C"))
	     (assert-true (is-process "CIAO"))
	     (assert-false (is-process "ciAO"))	; fallisce, servono regexp
	     (assert-false (is-process "ciao"))
	     (assert-false (is-process "&")))


(run-tests)
