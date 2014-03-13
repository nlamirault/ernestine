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


(defsystem ernestine-gui
  :name "ernestine-gui"
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :version "0.4"
  :licence "Lisp Lesser GNU General Public License"
  :description "Ernestine GUI."
  :depends-on (:mcclim
               :cl-log
               :cl-xspf
	       ;; FIXME: upgrade to cl-lastfm
               ;;:cl-audioscrobbler
               :ernestine)
  :components
  ((:module :lib
            :components
            ((:file "file-selector")))
   (:module :src
            :depends-on (:lib)
            :components
            ((:module :gui
                      :components
                      ((:file "package")
                       (:file "specials" :depends-on ("package"))
                       (:file "tools" :depends-on ("specials"))
                       (:file "about" :depends-on ("tools"))
                       (:file "help" :depends-on ("tools"))
                       (:file "scan" :depends-on ("package"))
                       (:file "playlist" :depends-on ("tools" "specials"))
                       (:file "log" :depends-on ("package"))
                       (:file "player" :depends-on ("about"
                                                    "help"
                                                    "scan"
                                                    "log"
                                                    "specials"
                                                    "playlist"))
                       (:file "commands" :depends-on ("player"))
                       ))))))
