;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ernestine-web.asd
;;;; Purpose:       ASDF definition for Ernestine
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :asdf)



(defsystem ernestine-web
  :name "ernestine-web"
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :version "0.4"
  :licence "Lisp Lesser GNU General Public License"
  :description "Ernestine web radio."
  :depends-on (:hunchentoot
               :html-template
               :cl-xspf
               :ernestine)
  :components
  ((:module :src
            :components
            ((:module :web
                      :components ((:file "package")
                                   (:file "specials" :depends-on ("package"))
                                   (:file "web" :depends-on ("specials"))))))))

