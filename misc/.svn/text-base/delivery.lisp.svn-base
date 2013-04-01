;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delivery.lisp
;;;; Purpose:       Make executable.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************

(in-package :cl-user)


(require 'ernestine-gui)


(defparameter *target-directory* "/tmp/")


(defun make-ernestine-executable ()
  "Creates an Ernestine McClim player executable."
  (sb-ext:save-lisp-and-die
   (merge-pathnames "ernestine" *target-directory*)
   :executable t
   :toplevel (lambda ()
               (ernestine-gui:player)
               (sb-ext:quit :unix-status 0))))

