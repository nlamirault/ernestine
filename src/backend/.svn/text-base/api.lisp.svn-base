;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.lisp
;;;; Purpose:       Backend's API
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



(defclass backend ()
  ()
  (:documentation "All backend herites this class"))


(defgeneric make-artist (backend artist-name albums)
  (:documentation "Create a music artist named ARTIST-NAME. ALBUMS is
an hashtable which contains albums of the artist.
Throws an EXISTING-ARTIST-ERROR if NAME is already in database."))


(defgeneric delete-artist (backend artist-name)
  (:documentation "Delete the artist identified by ARTIST-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown."))
  

(defgeneric make-album (backend artist-name album-name style date cover songs)
  (:documentation "Create a music album named ALBUM-NAME for the artiste
identified by ARTIST-NAME. STYLE represents in which category the album is
classified, DATE is the vendor date and SONGS an hashtable of album's songs.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an EXISTING-ALBUM-ERROR if ALBUM-NAME is already."))


(defgeneric delete-album (backend artist-name album-name)
  (:documentation  "Delete the album identified by ALBUM-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown."))


(defgeneric make-song (backend artist-name album-name song-title rank time filename)
  (:documentation "Create a song named SONG-TITLE in the album identified by
ALBUM-NAME.
RANK is the song's position in the album, and TIME the elapsed time.
FILENAME is the pathname of the song.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown.
Throws an EXISTING-SONG-ERROR if SONG-TITLE is already."))


(defgeneric delete-song (backend artist-name album-name song-title)
  (:documentation "Delete the song identified by SONG-TITLE in the
album ALBUM-NAME of artist ARTIST-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown.
Throws an UNKNOWN-SONG-ERROR if SONG-TITLE is unknown."))


(defgeneric get-artists (backend)
  (:documentation "Retrieve an hashtable which contains artists. artists's names
are the keys of the hashtable."))

(defgeneric get-artist (backend artist-name)
  (:documentation "Retrieve the artist identified by ARTIST-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown."))


(defgeneric get-artist-albums (backend artist-name)
  (:documentation "Retrieve the albums of artist named ARTIST-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown."))


(defgeneric get-artist-album (backend artist-name album-name)
  (:documentation "Retrieve the album named ALBUM-NAME, owned by artist
ARTIST-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown."))


(defgeneric get-album-songs (backend artist-name album-name)
  (:documentation "Retrieve the songs of album named ALBUM-NAME owned by
artist ARTIST-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown."))


(defgeneric get-album-song (backend artist-name album-name song-title)
  (:documentation "Retrieve the song named SONG-TITLE in album ALBUM-NAME
of artist ARTIST-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown.
Throws an UNKNOWN-SONG-ERROR if SONG-TITLE is unknown."))


(defgeneric get-album-song-by-rank (backend artist-name album-name rank)
  (:documentation "Retrieve the song in position RANK in album ALBUM-NAME
of artist ARTIST-NAME.
Throws an UNKNOWN-ARTIST-ERROR if ARTIST-NAME is unknown.
Throws an UNKNOWN-ALBUM-ERROR if ALBUM-NAME is unknown.
Throws an UNKNOWN-SONG-ERROR if SONG-TITLE is already."))


(defmacro do-artists ((artist) backend &body body)
  "Iterates over the artists of the BACKEND's database."
  `(let* ((artists (get-artists ,backend))
          (data (sort (loop for key being the hash-key of artists
                         collect key)
                      #'string-lessp)))
     (loop for name in data
        as ,artist = (gethash name artists)
        do ,@body)))

  

(defmacro do-albums ((album) backend artist-name &body body)
  "Retrieve artist named ARTIST-NAME and iterates over his albums extract from
the BACKEND's database."
  `(let* ((albums (get-artist-albums ,backend ,artist-name))
          (data (sort (loop for key being the hash-key of albums
                         collect key)
                      #'string-lessp)))
     (loop for name in data
          as ,album = (gethash name albums)
        do ,@body)))


(defmacro do-songs ((song) backend artist-name album-name &body body)
  "Retrieve artist's album named ARTIST-NAME and ALBUM-NAME, and iterates
over the album's songs extract from BACKEND's database."
  `(let ((songs (get-album-songs ,backend ,artist-name ,album-name)))
     (loop for key being the hash-key of songs
        as ,song = (gethash key songs)
        do ,@body)))


(defgeneric get-genre (backend)
  (:documentation "Retrieve a list of music genres from all albums."))


(defmethod get-genre ((backend backend))
  (let ((db-genres '()))
    (do-artists (artist) backend
      (do-albums (album) backend (artist-name artist)
        (unless (member (album-style album) db-genres :test #'string-equal)
          (push (album-style album) db-genres))))
    db-genres))


(defgeneric get-music-by-genre (backend genres)
  (:documentation "Retrieve all songs with a music genre.
GENRES is a list a different music genre."))


(defmethod get-music-by-genre ((backend backend) genres)
  (let ((selection '()))
    (do-artists (artist) backend
      (do-albums (album) backend (artist-name artist)
        (when (find (album-style album) genres :test #'string-equal)
;;           (format t "~&~A : ~A" 
;;                   (album-name album) (album-style album))
          (do-songs (song) backend (artist-name artist) (album-name album)
            (push song selection)))))
    selection))
         


(defgeneric print-database (backend &key stream artistp albump songp)
  (:documentation "Print to the stream the database.
If ARTISTP, print the artists' names.
If ALBUMP, print the albums' names.
If SONGP, print the songs' names."))


(defmethod print-database ((backend backend)
                           &key (stream *standard-output*)
                           artistp albump songp)
  (let ((*print-ernestine-database* t))
    (format stream "Music Database~%")
    (when artistp
      (do-artists (artist) backend
        (print-object artist stream)
        (when albump
          (do-albums (album) backend (artist-name artist)
            (print-object album stream)
            (when songp
              (do-songs (song) backend (artist-name artist) (album-name album)
                (print-object song stream)))))))))

