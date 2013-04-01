;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ernestine.lisp
;;;; Purpose:       Main music database backend
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :ernestine-database)




;; -----------
;; Interface
;; -----------


(defun get-backend (type &optional directory)
  "Get a backend from TYPE.
Throws an UNKNOWN-BACKEND-ERROR is backend type TYPE is unknown"
  (case type
    (:prevalence (make-prevalence-backend directory))
    (otherwise (error 'unknown-backend-error :backend-type type))))
                 


(defmacro with-backend ((backend type directory) &body body)
  "Macro which defined a new BACKEND and executes BODY."
  `(let ((,backend (get-backend ,type ,directory)))
     ,@body))




