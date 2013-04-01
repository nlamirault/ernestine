;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prevalence.lisp
;;;; Purpose:       Backend which use the cl-prevalence tool
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



;; ---------
;; Database
;; ---------


(defclass database ()
  ((artists :accessor database-artists
            :initform (make-hash-table :test 'equal)
            :documentation "Hashtable of music artists"))
  (:documentation "Music Database"))


(defun tx-create-music (system)
  "Create the music database"
   (setf (get-root-object system :music)
         (make-instance 'database)))


(defun init-music-system (system)
  "Initialization of the database system"
  (unless (get-root-object system :music)
    (execute system (make-transaction 'tx-create-music)))
  system)


(defun get-database (system)
  "Retreive music database from system"
  (let ((music (get-root-object system :music)))
    (database-artists music)))



;; -------------
;; Transactions 
;; -------------


(defun tx-create-artist (system artist-name albums)
  "Transaction for creating a music artist named ARTIST-NAME, which have ALBUMS.
Updates the database system and return the artist object.
Throws an EXISTING-ARTIST-ERROR if NAME is already in database."
  (let ((music (get-database system)))
    (if (gethash artist-name music)
        (error 'existing-artist-error :artist-name artist-name)
        (let* ((data (if albums albums (make-hash-table :test #'equal)))
               (artist (make-instance 'artist :name artist-name :albums data)))
          (setf (gethash artist-name music) artist)
          artist))))


(defun tx-delete-artist (system artist-name)
  "Transaction for deleting the artist identified by ARTIST-NAME from the database.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown from database."
  (let* ((music (get-database system))
         (artist (gethash artist-name music)))
    (if artist
        (remhash artist-name music)
        (error 'unknown-artist-error :artist-name artist-name))))


(defun tx-create-album (system artist-name album-name style date cover songs)
  "Transaction for creating an album for this artist identified by ARTIST-NAME.
Updates the database system and return the album object.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown from database.
Throws an EXISTING-ALBUM-ERROR if ALBUM-NAME is already in database."
  (let* ((music (get-database system))
         (artist (gethash artist-name music)))
    (if artist
        (if (gethash album-name (artist-albums artist))
            (error 'existing-album-error
                   :album-name album-name :artist-name artist-name)
            (let* ((data (if songs songs (make-hash-table :test #'equal)))
                   (album (make-instance 'album
                                         :name album-name
                                         :artist artist-name
                                         :style style
                                         :date date
                                         :cover cover
                                         :songs data)))
              (setf (gethash album-name (artist-albums artist)) album)
              album))
        (error 'unknown-artist-error :artist-name artist-name))))


(defun tx-delete-album (system artist-name album-name)
  "Transaction for deleting the album identified by ALBUM-NAME from the database.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown from database.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown from database."
  (let* ((music (get-database system))
         (artist (gethash artist-name music)))
    (if artist
        (let ((album (gethash album-name (artist-albums artist))))
          (if album
              (remhash album-name (artist-albums artist))
              (error 'unknown-album-error
                     :artist-name artist-name
                     :album-name album-name)))
        (error 'unknown-artist-error :artist-name artist-name))))


(defun tx-create-song (system artist-name album-name song-title rank time filename)
  "Transaction for creating a song for this album identified by ALBUM-NAME.
Updates the database system and return the song object.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown from database.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown from database
Throws an EXISTING-SONG-ERROR if SONG-TITLE is already in database."
  (let* ((music (get-database system))
         (artist (gethash artist-name music)))
    (if artist
        (let ((album (gethash album-name (artist-albums artist))))
          (if album
              (if (gethash song-title (album-songs album))
                  (error 'existing-song-error
                         :album-name album-name
                         :song-name song-title)
                  (let ((song (make-instance 'song
                                             :title song-title
                                             :album album-name
                                             :artist artist-name
                                             :rank rank
                                             :time time
                                             :filename filename)))
                    (setf (gethash song-title (album-songs album)) song)
                    song))
              (error 'unknown-album-error
                     :artist-name artist-name
                     :album-name album-name)))
        (error 'unknown-artist-error :artist-name artist-name))))


(defun tx-delete-song (system artist-name album-name song-title)
  "Transaction for deleting the song identified by SONG-TITLE from the database.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown from database.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown from database.
Throws an UNKNOWN-SONG-ERROR if SONG-TITLE is unknown from database."
  (let* ((music (get-database system))
         (artist (gethash artist-name music)))
    (if artist
        (let ((album (gethash album-name (artist-albums artist))))
          (if album
              (let ((song (gethash song-title (album-songs album))))
                (if song
                    (remhash song-title (album-songs album))
                    (error 'unknown-song-error
                           :album-name album-name
                           :song-name song-title)))
              (error 'unknown-album-error
                     :artist-name artist-name
                     :album-name album-name)))
        (error 'unknown-artist-error :artist-name artist-name))))


;; -----------
;; Operations
;; -----------


(defclass prevalence (backend)
  ((system :initform nil
           :initarg :system
           :accessor prevalence-system
           :documentation "System prevalence")
   (directory :initform nil
              :initarg :directory
              :accessor prevalence-directory
              :documentation "Directory name of the log transaction's file
of cl-prevalence"))
  (:documentation "Database backend which use the cl-prevalence tool"))



(defun make-prevalence-backend (directory)
  "Create the prevalence system backend, and initialize the prevalence
system. DIRECTORY will contain the log transaction's file of cl-prevalence.
Throws a sb-int:simple-file-error if DIRECTORY is unknown."
  (make-instance 'prevalence
                 :system (init-music-system (make-prevalence-system
                                             (pathname directory)))
                 :directory directory))


(defmethod make-artist ((prevalence prevalence) artist-name albums)
  (execute (prevalence-system prevalence)
           (make-transaction 'tx-create-artist artist-name albums)))


(defmethod delete-artist ((prevalence prevalence) artist-name)
  (execute (prevalence-system prevalence)
           (make-transaction 'tx-delete-artist artist-name)))


(defmethod make-album ((prevalence prevalence) artist-name album-name style date cover songs)
  (execute (prevalence-system prevalence)
           (make-transaction 'tx-create-album
                             artist-name album-name style date cover songs)))


(defmethod delete-album ((prevalence prevalence) artist-name album-name)
  (execute (prevalence-system prevalence)
           (make-transaction 'tx-delete-album artist-name album-name)))


(defmethod make-song ((prevalence prevalence)
                      artist-name album-name song-title rank time filename)
  (execute (prevalence-system prevalence)
           (make-transaction 'tx-create-song
                             artist-name album-name song-title rank time filename)))


(defmethod delete-song ((prevalence prevalence) artist-name album-name song-title)
  (execute (prevalence-system prevalence)
           (make-transaction 'tx-delete-song
                             artist-name album-name song-title)))



;; -------
;; Macros
;; -------


(defmacro with-artist ((artist prevalence artist-name) &body body)
  "Macro which find ARTIST by his name ARTIST-NAME and executes BODY.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown from database."
  `(let* ((music (get-database (prevalence-system ,prevalence)))
          (,artist (gethash ,artist-name music)))
    (if ,artist
        ,@body
        (error 'unknown-artist-error :artist-name ,artist-name))))


(defmacro with-album ((album prevalence artist-name album-name) &body body)
  "Macro which find ALBUM by his name ALBUM-NAME and executes BODY.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown from database.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown from database."
  `(with-artist (artist ,prevalence ,artist-name)
     (let ((,album (gethash ,album-name (artist-albums artist))))
       (if ,album
           ,@body
           (error 'unknown-album-error
                  :artist-name ,artist-name :album-name ,album-name)))))


(defmacro with-song ((song prevalence artist-name album-name song-title) &body body)
  "Macro which find SONG by his name SONG-NAME and executes BODY.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown from database.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown from database.
Throws an UNKNOWN-SONG-ERROR if SONG-NAME is unknown from database."
  `(with-album (album ,prevalence ,artist-name ,album-name)
     (let ((,song (gethash ,song-title (album-songs album))))
       (if ,song
           ,@body
           (error 'unknown-song-error
                  :album-name (album-name album) :song-name ,song-title)))))



;; ----------
;; Accessors
;; ----------


(defmethod get-artists ((prevalence prevalence))
  (get-database (prevalence-system prevalence)))


(defmethod get-artist ((prevalence prevalence) artist-name)
  (with-artist (artist prevalence artist-name)
    artist))


(defmethod get-artist-albums ((prevalence prevalence) artist-name)
  (with-artist (artist prevalence artist-name)
    (artist-albums artist)))


(defmethod get-artist-album ((prevalence prevalence) artist-name album-name)
  (gethash album-name (get-artist-albums prevalence artist-name)))


(defmethod get-album-songs ((prevalence prevalence) artist-name album-name)
  (with-album (album prevalence artist-name album-name)
    (album-songs album)))


(defmethod get-album-song ((prevalence prevalence) artist-name album-name song-title)
  (gethash song-title (get-album-songs prevalence artist-name album-name)))


(defmethod get-album-song-by-rank ((prevalence prevalence) artist-name album-name rank)
  (let ((songs (get-album-songs prevalence artist-name album-name)))
    (loop for key being the hash-key of songs
       as song = (gethash key songs)
       when (string-equal rank (song-rank song))
       return song)))


