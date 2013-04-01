;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Ernestine GUI Player general informations.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :ernestine-gui)


(defparameter *ernestine-player* nil)

(defparameter *music-bigb* nil)

(defparameter *hilight-color* (clim:make-rgb-color 0.8 0.8 1.0))


;; (defparameter *player-command* "/usr/bin/sox"
;;   "The Swiss army knife of sound processing program.")


;; (unless (boundp '+player-args+)
;;   (defconstant +player-args+ (list "-t" "ossdsp" "/dev/dsp")))


;; (defparameter *volume-command* "/usr/bin/aumix"
;;   "The mixer control program.")


;; (unless (boundp '+volume-args+)
;;   (defconstant +volume-args+ "-v"))


;; For Last.FM

(defparameter *last-fm* nil "A last-fm-client object.")


(unless (boundp '+cl-audioscrobbler-client-id+)
  (defconstant +cl-audioscrobbler-client-id+ "ern"))


(unless (boundp '+cl-audioscrobbler-client-version+)
  (defconstant +cl-audioscrobbler-client-version+ "0.1"))



;; Dev

(defparameter *debug* nil "If T, active some logs.")

