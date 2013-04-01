;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          log.lisp
;;;; Purpose:       Log events from Ernestine Music player
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault.
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :ernestine-gui)



(defun init-log (directory)
  "Set the log system."
  (setf (cl-log:log-manager)
        (make-instance 'cl-log:log-manager
                       :message-class 'cl-log:formatted-message))
  (cl-log:start-messenger 'cl-log:text-file-messenger
                          :filename (concatenate 'string
                                                 directory
                                                 "/log.txt")))
