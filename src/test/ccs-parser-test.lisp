;;;; ccs-parser-test.lisp

(defpackage #:ritucci-msc-ccs-parser-test
  (:use #:common-lisp #:ritucci-msc-ccs-parser #:lisp-unit))


(in-package #:ritucci-msc-ccs-parser-test)


(define-test is-action
	     (assert-true (is-action "&"))
	     (assert-false (is-action "!&"))
	     (assert-false (is-action "&!"))
	     (assert-false (is-action "&&")))


(define-test is-label
	     (assert-true (is-label "a"))
	     (assert-true (is-label "!a"))
	     (assert-true (is-label "label"))
	     (assert-true (is-label "!label"))
	     (assert-false (is-label "!!a"))
	     (assert-false (is-label "!a!"))
	     (assert-false (is-label "a!"))
	     (assert-false (is-label "a!!"))
	     (assert-false (is-label "!!label"))
	     (assert-false (is-label "!la!bel"))
	     (assert-false (is-label "!label!"))
	     (assert-false (is-label "&"))
	     (assert-false (is-label "*"))
	     (assert-false (is-label "Label"))
	     (assert-false (is-label "LABEL"))
	     (assert-false (is-label "labeL"))
	     (assert-false (is-label "!labeL"))
	     (assert-false (is-label "!Label")))


(define-test is-process
	     (assert-true (is-process "P"))
	     (assert-true (is-process "PROCESS"))
	     (assert-true (is-process "Process"))
	     (assert-false (is-process "process"))
	     (assert-false (is-process "pROCESS"))
	     (assert-false (is-process "!P")))


(run-tests)
