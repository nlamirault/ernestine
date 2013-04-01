;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          suite.lisp
;;;; Purpose:       Unit Tests Suite for ernestine-database
;;;; Programmer:    Nicolas Lamirault <lam@tuxfamily.org>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :ernestine-database-tests)


(defun run-database-tests (config-file)
  "Run the Ernestine Unit Tests using CONFIG-FILE."
  (lift:run-tests :config config-file))

