;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ernestine.asd
;;;; Purpose:       ASDF definition for ernestine
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



(defsystem ernestine
  :name "ernestine"
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :version "0.4"
  :licence "Lisp Lesser GNU General Public License"
  :description "Music Organizer"
  :long-description "Ernestine is a music organizer. It consists
of a database backend which organizes music by artists, and albums.
Therefore, you can launch differents frontend : web, curses, ..."
  :depends-on (:split-sequence
               :cl-prevalence
               :cl-ppcre
               :drakma)
  :components
  ((:module :lib                                                                  
            :components
            ((:file "covers")))
   (:module :src
            :depends-on (:lib)
            :components
            ((:module :backend
                      :components
                      ((:file "package")
                       (:file "specials" :depends-on ("package"))
                       (:file "errors" :depends-on ("package"))
                       (:file "obj" :depends-on ("specials"))
                       (:file "api" :depends-on ("specials"))
                       (:file "prevalence" :depends-on ("api" "obj" "errors"))
                       (:file "common" :depends-on ("prevalence"))))
             (:module :tools
                      :components ((:file "package")
                                   (:file "specials" :depends-on ("package"))
                                   (:file "music" :depends-on ("specials"))
                                   (:file "io" :depends-on ("music"))
                                   (:file "misc" :depends-on ("specials")))
                      :depends-on (:backend))))))
