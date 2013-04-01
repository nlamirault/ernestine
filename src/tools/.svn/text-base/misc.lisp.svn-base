;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          misc.lisp
;;;; Purpose:       Some Ernestine miscealinous tools.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :ernestine-tools)


(defun get-ernestine-directory ()
  "Get the home directory of Ernestine."
  (let ((directory (concatenate 'string
                                (sb-ext:posix-getenv "HOME")
                                "/"
                                ".ernestine/")))
    (ensure-directories-exist directory)))


(defun check-installation ()
  "Check installation for Ernestine."
  (format t "Check Ernestine installation :")
  (loop for data in (list *file-info* *mp3-info* *ogg-info* *volume-command*
                          *player-command*)
     do
       (format t "~&Verify ~A ..." data)
       (unless (file-write-date data)
         (error "Check Ernestine installation : ~A" data)))
  t)
