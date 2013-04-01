;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Packages definition for Ernestine database backend.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :ernestine-database
  (:use :cl :cl-prevalence)
  (:documentation "Prevalence backend of ernestine")
  (:export #:get-backend
           #:with-backend

           #:backend

           ;; prevalence backend
           
           #:prevalence
           #:make-prevalence-backend

           ;; database
           
           #:artist
           #:artist-name
           
           #:album
           #:album-name
           #:album-artist
           #:album-style
           #:album-date
           #:album-cover
           
           #:song
           #:song-title
           #:song-album
           #:song-artist
           #:song-rank
           #:song-time
           #:song-filename
           
           #:make-artist
           #:delete-artist
           #:make-album
           #:delete-album
           #:make-song
           #:delete-song

           ;; tools

           #:do-artists
           #:do-albums
           #:do-songs
           
           #:get-artists
           #:get-artist
           #:get-artist-albums
           #:get-artist-album
           #:get-album-songs
           #:get-album-song
           #:get-album-song-by-rank
           #:get-genre
           #:get-music-by-genre

           #:add-album-cover
           
           ;; conditions

           #:unknown-backend-error
           #:unknown-artist-error
           #:existing-artist-error
           #:unknown-album-error
           #:existing-album-error
           #:unknown-song-error
           #:existing-song-error

           #:print-database

           #:*print-ernestine-database*

   ))

