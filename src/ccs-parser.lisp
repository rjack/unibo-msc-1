;;;; ccs-parser.lisp 

;;; Parser per la sintassi CCS.

;; Copyright (c) 2008 Giacomo Ritucci.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY GIACOMO RITUCCI ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN
;; NO EVENT SHALL GIACOMO RITUCCI BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(defpackage #:ritucci-msc-ccs-parser
  (:use #:common-lisp #:ext #:regexp)
  (:export #:parse))


(in-package #:ritucci-msc-ccs-parser)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *label-pattern*
  "!\\{0,1\\}[a-z][a-z0-9]*")

(defparameter *process-pattern*
  "[A-Z][a-zA-Z0-9]*")

(defparameter *transition-pattern*
  "\\(.\\{1,\\}\\) -\\(.\\{1,\\}\\)-> \\(.\\{1,\\}\\)")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro full-pattern-of (pattern)
  `(string-concat "^" ,pattern "$"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse (text)
  "Accetta una stringa e tenta di costruire un albero sintattico a partire
  dalla stringa data.
  Ritorna nil se non riesce, l'albero sintattico se riesce."
  (cond ((parse-transition text))
	((parse-definition text))
	((parse-choice text))
	((parse-composition text))
	((parse-prefixing text))
	((parse-relabelling text))
	((parse-restriction text))
	((parse-process text))
	((parse-label text))
	(t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun parse-transition (text)
  "Tenta di costruire l'albero sinttico di una transizione.
  Ritorna l'albero se riesce, NIL altrimenti."
  (multiple-value-bind
    (whole-match left-match center-match right-match)
    (match (full-pattern-of *transition-pattern*) text)

    (if (and whole-match left-match center-match right-match)
      ;; match-string solo per debug, bisogna chiamare ricorsivamente parse.
      (list (match-string text left-match)
	    (match-string text center-match)
	    (match-string text right-match))
      nil)))


(defun is-action (text)
  "Ritorna T se text è un'etichetta di un'azione, il suo complementar o tau;
  NIL altrimenti"
  (or (string= text "&")
      (is-label text)))


(defun is-label (text)
  "Ritorna T se text è un'etichetta di un'azione o il suo complementare; NIL
  altrimenti"
  (cond ((match (full-pattern-of *label-pattern*) text) t)
	(t nil)))


(defun is-process (text)
  "Ritorna T se text è un processo; NIL altrimenti"
  (cond ((string= text "0") t)
	((match (full-pattern-of *process-pattern*) text) t)
	(t nil)))
