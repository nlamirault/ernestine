;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          covers.lisp
;;;; Purpose:       Search for CD covers
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(cl:defpackage :cl-covers
  (:use #:common-lisp)
  (:export #:search-album-covers))


(in-package :cl-covers)



(unless (boundp '+cover-search+)
  (defconstant +cover-search+
    "http://www.slothradio.com/covers/?adv=&artist=~A&album=~A"))


(unless (boundp '+ws-amazon+)
  (defconstant +ws-amazon+ "<img src=\"http://.*.images-amazon.com/images/.*\""))


(defun search-album-covers (artist-name &optional (album-name ""))
  (let ((url (cl-ppcre:regex-replace-all " "
                                         (format nil
                                                 +cover-search+
                                                 artist-name
                                                 album-name)
                                         "+"))
        (results))
    (format t "URI~%~A~%" url)
    (handler-case
        (multiple-value-bind (body status-code headers uri stream must-close)
            (drakma:http-request url)
          (declare (ignore headers uri stream must-close))
          (when (and status-code (= status-code 200))
            (with-input-from-string (stream body)
              (loop as line = (read-line stream nil)
                 until (null line)
                 do (cl-ppcre:do-matches (s e +ws-amazon+ line)
                      (let ((end (search "width=" line)))
                        (when end
                          (let ((cover (subseq line (+ 10 s) (- end 2))))
                            (push cover results)
                            (format t "~&Image: ~A" cover)))))))))
      (puri:uri-parse-error (condition)
        (warn "Fetch cover URI problem : ~A" condition)))
    (reverse results)))
