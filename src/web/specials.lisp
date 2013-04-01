;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Some specials variables of the web system.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :ernestine-web)


(defparameter *version* (asdf:component-version (asdf:find-system "ernestine")))


(defparameter *debug* nil "When T active some log.")

(defparameter *ernestine-directory*
  (namestring
   (asdf:component-relative-pathname (asdf:find-system :ernestine-web)))
  "Directory with contains Ernestine source files.")


(defparameter *port* 9090 "The default port of the listener.")


(unless (boundp '+templates-directory+)
  (defconstant +templates-directory+ "html/"
    "Name of the directory which contains HTML templates."))


(unless (boundp '+images-directory+)
  (defconstant +images-directory+ "img/"
    "Name of the directory which contains images."))


(unless (boundp '+stylesheets-directory+)
  (defconstant +stylesheets-directory+ "css/"
    "Name of the directory which contains the CSS."))


(unless (boundp '+playlist-name+)
  (defconstant +playlist-name+ "playlist.xspf"))


(defparameter *music-directory* "/opt/music/"
  "Directory which contains music files.")


(defparameter *playlist-directory* "/tmp/"
  "Directory where Ernestine creates playlist.")