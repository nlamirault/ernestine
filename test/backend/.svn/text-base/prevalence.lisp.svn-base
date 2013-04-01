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


(in-package :ernestine-database-tests)



(lift:deftestsuite ernestine-database-test ()
  ()
  (:documentation "Unit Test suite for Ernestine database backend."))


(lift:addtest (ernestine-database-test)
  backend-type-test
  (:documentation "Test an available database backend.")
  (let ((db (get-backend :prevalence "/tmp/")))
    (lift:ensure (eq (type-of db) 'prevalence))))


(lift:addtest (ernestine-database-test)
  unknown-backend-type-test
  (:documentation "Test an unknown database backend.")
    (lift:ensure-condition 'unknown-backend-error
      (get-backend :unknown "/tmp/")))


(lift:addtest (ernestine-database-test)
  add-artist-test
  (:documentation "Test to add a new artist to the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (data (make-artist backend (format nil "artist-~A" (gensym)) nil)))
    (lift:ensure (eq (type-of data) 'artist))))


(lift:addtest (ernestine-database-test)
  delete-artist-test
  (:documentation "Test to delete an artist from the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (name (format nil "artist-~A" (gensym))))
    (make-artist backend name nil)
    (lift:ensure (delete-artist backend name))))


(lift:addtest (ernestine-database-test)
  add-existing-artist-test
  (:documentation "Test to add an existing artist to the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (name (format nil "artist-~A" (gensym))))
    (make-artist backend name nil)
    (lift:ensure-condition 'existing-artist-error
      (make-artist backend name nil))))


(lift:addtest (ernestine-database-test)
  delete-unknown-artist-test
  (:documentation "Test to delete an unknown artist from the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (name (format nil "artist-~A" (gensym))))
    (lift:ensure-condition 'unknown-artist-error
      (delete-artist backend name))))


(lift:addtest (ernestine-database-test)
  add-album-test
  (:documentation "Test to add a new album to the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (artist-name (format nil "artist-~A" (gensym)))
         (album-name (format nil "album-~A" (gensym))))
    (make-artist backend artist-name nil)
    (let ((data (make-album backend artist-name album-name "rock" "2007" nil)))
      (lift:ensure (eq (type-of data) 'album)))))


(lift:addtest (ernestine-database-test)
  add-existing-album-test
  (:documentation "Test to add an existing album to the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (artist-name (format nil "artist-~A" (gensym)))
         (album-name (format nil "album-~A" (gensym))))
    (make-artist backend artist-name nil)
    (make-album backend artist-name album-name "rock" "2007" nil)
    (lift:ensure-condition 'existing-album-error
      (make-album backend artist-name album-name "rock" "2007" nil))))


(lift:addtest (ernestine-database-test)
  delete-album-test
  (:documentation "Test to delete an album from the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (artist-name (format nil "artist-~A" (gensym)))
         (album-name (format nil "album-~A" (gensym))))
    (make-artist backend artist-name nil)
    (make-album backend artist-name album-name "rock" "2007" nil)
    (lift:ensure (delete-album backend artist-name album-name))))


(lift:addtest (ernestine-database-test)
  delete-unknown-album-test
  (:documentation "Test to delete an unknown album from the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (artist-name (format nil "artist-~A" (gensym)))
         (album-name (format nil "album-~A" (gensym))))
    (make-artist backend artist-name nil)
    (lift:ensure-condition 'unknown-album-error
      (delete-album backend artist-name album-name))))


(lift:addtest (ernestine-database-test)
  add-song-test
  (:documentation "Test to add a new song to the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (artist-name (format nil "artist-~A" (gensym)))
         (album-name (format nil "album-~A" (gensym)))
         (song-name (format nil "song-~A" (gensym))))
    (make-artist backend artist-name nil)
    (make-album backend artist-name album-name "rock" "2007" nil)
    (let ((data
           (make-song backend artist-name album-name song-name "1" "25" "foo.mp3")))
      (lift:ensure (eq (type-of data) 'song)))))


(lift:addtest (ernestine-database-test)
  delete-song-test
  (:documentation "Test to delete an existing song from the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (artist-name (format nil "artist-~A" (gensym)))
         (album-name (format nil "album-~A" (gensym)))
         (song-name (format nil "song-~A" (gensym))))
    (make-artist backend artist-name nil)
    (make-album backend artist-name album-name "rock" "2007" nil)
    (make-song backend artist-name album-name song-name "1" "25" "foo.mp3")
    (lift:ensure (delete-song backend artist-name album-name song-name))))


(lift:addtest (ernestine-database-test)
  add-existing-song-test
  (:documentation "Test to add an existing song to the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (artist-name (format nil "artist-~A" (gensym)))
         (album-name (format nil "album-~A" (gensym)))
         (song-name (format nil "song-~A" (gensym))))
    (make-artist backend artist-name nil)
    (make-album backend artist-name album-name "rock" "2007" nil)
    (make-song backend artist-name album-name song-name "1" "25" "foo.mp3")
    (lift:ensure-condition 'existing-song-error
      (make-song backend artist-name album-name song-name "1" "25" "foo.mp3"))))


(lift:addtest (ernestine-database-test)
  delete-unknown-song-test
  (:documentation "Test to delete an unknown song from the database.")
  (let* ((backend (get-backend :prevalence "/tmp/foo/"))
         (artist-name (format nil "artist-~A" (gensym)))
         (album-name (format nil "album-~A" (gensym)))
         (song-name (format nil "song-~A" (gensym))))
    (make-artist backend artist-name nil)
    (make-album backend artist-name album-name "rock" "2007" nil)
    (lift:ensure-condition 'unknown-song-error
      (delete-song backend artist-name album-name song-name))))


