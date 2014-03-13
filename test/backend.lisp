;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; ****************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prevalence.lisp
;;;; Purpose:       Unit Test for prevalence backend for ernestine
;;;; Programmer:    Nicolas Lamirault <lam@tuxfamily.org>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine-database-tests users are granted the rights to distribute and
;;;; use this software as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; ****************************************************************************


(in-package :ernestine-tests)


(define-test get-prevalence-backend
  (let ((db (ernestine-database:get-backend :prevalence "/tmp/")))
    (assert-true (eq (type-of db) 'ernestine-database:prevalence))))

(define-test get-invalid-backend
  (assert-error 'ernestine-database:unknown-backend-error
		(ernestine-database:get-backend :unknown "/tmp/")))

(defmacro with-prevalence-backend ((backend) &body body)
  `(let ((,backend (ernestine-database:get-backend :prevalence "/tmp/foo/")))
     ,@body))

(define-test add-artist
  (with-prevalence-backend (backend)
    (let* ((name (format nil "artist-~A" (gensym)))
	   (data (ernestine-database:make-artist backend name nil)))
      (assert-true (eq (type-of data) 'ernestine-database:artist)))))

(define-test delete-artist
  (with-prevalence-backend (backend)
    (let* ((name (format nil "artist-~A" (gensym))))
      (ernestine-database:make-artist backend name nil)
      (assert-true (ernestine-database:delete-artist backend name)))))

(define-test cant-add-existing-artist
  (with-prevalence-backend (backend)
    (let* ((name (format nil "artist-~A" (gensym))))
      (ernestine-database:make-artist backend name nil)
      (assert-error 'ernestine-database:existing-artist-error
		    (ernestine-database:make-artist backend name nil)))))

(define-test delete-unknown-artist
  (with-prevalence-backend (backend)
    (let* ((name (format nil "artist-~A" (gensym))))
      (assert-error 'ernestine-database:unknown-artist-error
		    (ernestine-database:delete-artist backend name)))))

(define-test add-album
  (with-prevalence-backend (backend)
    (let* ((artist-name (format nil "artist-~A" (gensym)))
	   (album-name (format nil "album-~A" (gensym))))
      (ernestine-database:make-artist backend artist-name nil)
      (let ((data (ernestine-database:make-album backend
						 artist-name
						 album-name
						 "rock" "2007" nil nil)))
	(assert-true (eq (type-of data) 'ernestine-database:album))))))

(define-test cant-add-existing-album
  (with-prevalence-backend (backend)
    (let* ((artist-name (format nil "artist-~A" (gensym)))
	   (album-name (format nil "album-~A" (gensym))))
      (ernestine-database:make-artist backend artist-name nil)
      (ernestine-database:make-album backend
				     artist-name
				     album-name
				     "rock" "2007" nil nil)
       (assert-error 'ernestine-database:existing-album-error
		     (ernestine-database:make-album backend
						    artist-name
						    album-name
						    "rock" "2007" nil nil)))))

(define-test delete-album
  (with-prevalence-backend (backend)
    (let* ((artist-name (format nil "artist-~A" (gensym)))
	   (album-name (format nil "album-~A" (gensym))))
      (ernestine-database:make-artist backend artist-name nil)
      (ernestine-database:make-album backend
				     artist-name
				     album-name
				     "rock" "2007" nil nil)
      (assert-true (ernestine-database:delete-album backend artist-name album-name)))))

(define-test delete-unknown-album
  (with-prevalence-backend (backend)
    (let* ((artist-name (format nil "artist-~A" (gensym)))
	   (album-name (format nil "album-~A" (gensym))))
      (ernestine-database:make-artist backend artist-name nil)
      (assert-error 'ernestine-database:unknown-album-error
		    (ernestine-database:delete-album backend artist-name album-name)))))

(define-test add-song
  (with-prevalence-backend (backend)
    (let* ((artist-name (format nil "artist-~A" (gensym)))
	   (album-name (format nil "album-~A" (gensym)))
	   (song-name (format nil "song-~A" (gensym))))
      (ernestine-database:make-artist backend artist-name nil)
      (ernestine-database:make-album backend
				     artist-name
				     album-name
				     "rock" "2007" nil nil)
      (let ((data
	     (ernestine-database:make-song backend artist-name album-name song-name "1" "25" "foo.mp3")))
	(assert-true (eq (type-of data) 'ernestine-database:song))))))


(define-test delete-song
  (with-prevalence-backend (backend)
    (let* ((artist-name (format nil "artist-~A" (gensym)))
	   (album-name (format nil "album-~A" (gensym)))
	   (song-name (format nil "song-~A" (gensym))))
      (ernestine-database:make-artist backend artist-name nil)
      (ernestine-database:make-album backend
				     artist-name
				     album-name
				     "rock" "2007" nil nil)
      (ernestine-database:make-song backend
				    artist-name
				    album-name
				    song-name
				    "1" "25" "foo.mp3")
      (assert-true (ernestine-database:delete-song backend
						   artist-name album-name song-name)))))

(define-test cant-add-existing-song
  (with-prevalence-backend (backend)
    (let* ((artist-name (format nil "artist-~A" (gensym)))
	   (album-name (format nil "album-~A" (gensym)))
	   (song-name (format nil "song-~A" (gensym))))
      (ernestine-database:make-artist backend artist-name nil)
      (ernestine-database:make-album backend
				     artist-name
				     album-name
				     "rock" "2007" nil nil)
       (ernestine-database:make-song backend
				    artist-name
				    album-name
				    song-name
				    "1" "25" "foo.mp3")
       (assert-error 'ernestine-database:existing-song-error
		     (ernestine-database:make-song backend
						   artist-name
						   album-name
						   song-name
						   "1" "25" "foo.mp3")))))

(define-test delete-unknown-song
  (with-prevalence-backend (backend)
    (let* ((artist-name (format nil "artist-~A" (gensym)))
	   (album-name (format nil "album-~A" (gensym)))
	   (song-name (format nil "song-~A" (gensym))))
      (ernestine-database:make-artist backend artist-name nil)
      (ernestine-database:make-album backend
				     artist-name
				     album-name
				     "rock" "2007" nil nil)
      (assert-error 'ernestine-database:unknown-song-error
		    (ernestine-database:delete-song backend
						    artist-name
						    album-name
						    song-name)))))
