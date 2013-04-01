;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          obj.lisp
;;;; Purpose:       Data Structures
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



(defclass artist ()
  ((name :initarg :name
         :initform ""
         :accessor artist-name
         :documentation "Music artist")
   (albums :initarg :albums 
           :initform (make-hash-table :test #'equal)
           :accessor artist-albums
           :documentation "Hashtable which contains albums of this artist"))
  (:documentation "Music artist"))


(defmethod print-object ((artist artist) stream)
  (if *print-ernestine-database*
      (format stream "~&Artist: ~A" (artist-name artist))
      (print-unreadable-object (artist stream :type t :identity t))))


(defclass album ()
  ((name :initarg :name
         :initform ""
         :accessor album-name
         :documentation "Album name")
   (artist :initarg :artist
           :initform ""
           :accessor album-artist
           :documentation "Name of the artist to which this album belongs")
   (style :initarg :style
          :initform ""
          :accessor album-style
          :documentation "Album music style")
   (date :initarg :date
         :initform ""
         :accessor album-date
         :documentation "Vendor date")
   (cover :initarg :cover
          :initform ""
          :accessor album-cover
          :documentation "An URI to retrieve cover art.")
   (songs :initarg :songs
          :initform (make-hash-table :test #'equal)
          :accessor album-songs
          :documentation "Hashtable of songs of this album"))   
  (:documentation "Music album"))


(defmethod print-object ((album album) stream)
  (if *print-ernestine-database*
      (with-slots (name style date cover) album
        (format stream "~&Title: ~A~&Style: ~A~&Date: ~A~&Cover: ~A"
                name style date cover))
      (print-unreadable-object (album stream :type t :identity t))))


(defclass song ()
  ((title :initarg :title
          :initform ""
          :accessor song-title
          :documentation "Song name")
   (album :initarg :album
          :initform ""
          :accessor song-album
          :documentation "Album name to which this song belongs")
   (artist :initarg :artist
           :initform ""
           :accessor song-artist
           :documentation "Name of the artist.")
   (rank :initarg :rank
         :initform ""
         :accessor song-rank
         :documentation "Rank of this song in the album")
   (time :initarg :time
         :initform ""
         :accessor song-time
         :documentation "Elapsed time of this song")
   (filename :initarg :filename
             :initform (error "Song must have a pathname")
             :accessor song-filename
             :documentation "Filename of the song."))
  (:documentation "Song of an album"))


(defmethod print-object ((song song) stream)
  (if *print-ernestine-database*
      (with-slots (title rank time filename) song
        (format stream "~&Title: ~A~&Rank: ~A~&Time: ~A~&File: ~A"
                title rank time filename))
      (print-unreadable-object (song stream :type t :identity t))))
