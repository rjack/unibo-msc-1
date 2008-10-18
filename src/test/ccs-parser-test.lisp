;;;; ccs-parser-test.lisp

(defpackage #:ritucci-msc-ccs-parser
  (:use #:common-lisp #:lisp-unit))


(in-package #:ritucci-msc-ccs-parser)


(define-test is-action-name
	     (assert-true (is-action-name "&"))
	     (assert-false (is-action-name "!&"))
	     (assert-false (is-action-name "&!"))
	     (assert-false (is-action-name "&&")))


(define-test is-label-name
	     (assert-true (is-label-name "a"))
	     (assert-true (is-label-name "!a"))
	     (assert-true (is-label-name "label"))
	     (assert-true (is-label-name "!label"))
	     (assert-true (is-label-name "a1"))
	     (assert-true (is-label-name "label6"))
	     (assert-true (is-label-name "!a1"))
	     (assert-true (is-label-name "!label6"))
	     (assert-false (is-label-name "2"))
	     (assert-false (is-label-name "!2"))
	     (assert-false (is-label-name "!!a"))
	     (assert-false (is-label-name "!a!"))
	     (assert-false (is-label-name "a!"))
	     (assert-false (is-label-name "a!!"))
	     (assert-false (is-label-name "!!label"))
	     (assert-false (is-label-name "!la!bel"))
	     (assert-false (is-label-name "!label!"))
	     (assert-false (is-label-name "&"))
	     (assert-false (is-label-name "*"))
	     (assert-false (is-label-name "Label"))
	     (assert-false (is-label-name "LABEL"))
	     (assert-false (is-label-name "labeL"))
	     (assert-false (is-label-name "!labeL"))
	     (assert-false (is-label-name "!Label")))


(define-test is-process-name
	     (assert-true (is-process-name "P"))
	     (assert-true (is-process-name "P1"))
	     (assert-true (is-process-name "PROCESS"))
	     (assert-true (is-process-name "PROCESS1"))
	     (assert-true (is-process-name "Process"))
	     (assert-false (is-process-name "process"))
	     (assert-false (is-process-name "process1"))
	     (assert-false (is-process-name "pROCESS"))
	     (assert-false (is-process-name "pROCESS1"))
	     (assert-false (is-process-name "3"))
	     (assert-false (is-process-name "!P")))


(run-tests)
