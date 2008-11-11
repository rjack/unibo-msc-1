;;;; ccs.lisp

;;; Oggetti che rappresentano espressioni CCS.

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

(defpackage #:ritucci-msc-ccs
  (:use #:common-lisp #:ext))


(in-package #:ritucci-msc-ccs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro if-all (test seq1 &body body)
  `())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +label-name-characters+
	     (string "abcdefghijklmnopqrstuvwxyz"))


(defconstant +action-name-characters+ (string "@"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ccs-expression ())


(defmethod is-valid (this ccs-label))


;; TODO ricordare che "t" (tau) è un'azione!
;; TODO ricordare che "!a" (a complementare) è un'azione e l'etichetta è "a"
(defclass ccs-action (ccs-expression)
  ((name
     :initarg :name
     :initform (error "name is missing")
     :reader name)
   (input
     :initarg :input
     :initform t
     :reader is-input)))


(defmethod initialize-instance :after (this ccs-action) &key


(defclass ccs-label (ccs-action))


(defclass ccs-process (ccs-expression))
;; TODO ricordare che "0" (nil) è un processo!


(defclass ccs-prefix (ccs-process))


(defclass ccs-relabeling (ccs-process))


(defclass ccs-restriction (ccs-process))


(defclass ccs-choice (ccs-process))


(defclass ccs-composition (ccs-process))


(defclass ccs-transition ())
