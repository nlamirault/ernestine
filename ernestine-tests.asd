;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ernestine-tests.asd
;;;; Purpose:       ASDF definition file for ernestine-tests package
;;;; Developer:     Nicolas Lamirault <lam@tuxfamily.org>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; $Id$
;;;;
;;;; *************************************************************************


(in-package #:cl-user)


(defpackage #:ernestine-tests-system 
  (:use #:cl #:asdf))


(in-package #:ernestine-tests-system)



(defsystem ernestine-tests
  :name "ernestine-tests"
  :author "Nicolas Lamirault <lam@tuxfamily.org>"
  :version "0.4"
  :licence "Lisp Lesser GNU General Public License"
  :description "Ernestine Unit Tests"
  :depends-on (:ernestine :lift)
  :components
  ((:module :test
            :components
            ((:module :backend
                      :components
                      ((:file "package")
                       (:file "prevalence" :depends-on ("package"))
                       (:file "suite" :depends-on ("prevalence"))
                       ))
             (:file "package")
             (:file "suite" :depends-on ("package" :backend))
             ))))
             





