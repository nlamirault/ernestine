;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package for Ernestine tools.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(defpackage :ernestine-tools
  (:use :cl :ernestine-database)
  (:documentation "Some tools using by Ernestine.")
  (:export #:scan-directory
           #:get-ernestine-directory
           #:check-installation

           #:*volume-command* 
           #:*player-command*
           #:+player-args+
           #:+volume-args+
           ))

