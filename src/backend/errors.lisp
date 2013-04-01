;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          errors.lisp
;;;; Purpose:       Exceptions
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :ernestine-database)




(define-condition ernestine-error (simple-error)
  ()
  (:documentation "Main music error"))



(define-condition unknown-backend-error (ernestine-error)
  ((backend-type :reader backend-type :initarg :backend-type))
  (:documentation "Unknown backend error")
  (:report (lambda (condition stream)
             (format stream "Unknown backend error ~A"
                     (backend-type condition)))))



(define-condition artist-error (ernestine-error)
  ((artist-name :reader artist-name :initarg :artist-name)))


(define-condition unknown-artist-error (artist-error)
  ()
  (:documentation "An unknown artist error")
  (:report (lambda (condition stream)
             (format stream "Unknown artist ~A"
                     (artist-name condition)))))


(define-condition existing-artist-error (artist-error)
  ()
  (:documentation "An existing artist error")
  (:report (lambda (condition stream)
             (format stream "Existing artist ~A"
                     (artist-name condition)))))


(define-condition album-error (ernestine-error)
  ((album-name :reader album-name :initarg :album-name)
   (artist-name :reader artist-name :initarg :artist-name)))


(define-condition unknown-album-error (album-error)
  ()
  (:documentation "An unknown album error")
  (:report (lambda (condition stream)
             (format stream "Unknown album ~A for the artist ~A"
                     (album-name condition)
                     (artist-name condition)))))


(define-condition existing-album-error (album-error)
  ()
  (:documentation "An existing album error")
  (:report (lambda (condition stream)
             (format stream "Existing album ~A for the artist ~A"
                     (album-name condition)
                     (artist-name condition)))))


(define-condition song-error (ernestine-error)
  ((song-name :reader song-name :initarg :song-name)
   (album-name :reader album-name :initarg :album-name)))


(define-condition unknown-song-error (song-error)
  ()
  (:documentation "An unknown song error")
  (:report (lambda (condition stream)
             (format stream "Unknown song ~A in ~A album"
                     (song-name condition)
                     (album-name condition)))))


(define-condition existing-song-error (song-error)
  ()
  (:documentation "An existing song error")
  (:report (lambda (condition stream)
             (format stream "Existing song ~A in this ~A album"
                     (song-name condition)
                     (album-name condition)))))



