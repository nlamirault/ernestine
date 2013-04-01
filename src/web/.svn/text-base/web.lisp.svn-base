;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          web.lisp
;;;; Purpose:       Web Frontend for ernestine
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



(defclass web-radio ()
  ((host :initform nil
         :initarg :host
         :accessor web-radio-host
         :documentation "URL of the web radio.")
   (port :initform nil
         :initarg :port
         :accessor web-radio-port
         :documentation "Port bind to the listener.")
   (xspf :initform nil
         :initarg :xspf
         :accessor web-radio-xspf
         :documentation "The XSPF system to generates playlist.")
   (main-dir :initform nil
             :initarg :main-dir
             :accessor web-radio-main-dir
             :documentation "Directory which contains HTML templates.")   
   (listener :initform nil
             :initarg :listener
             :accessor web-radio-listener
             :documentation "The web server for the Ernestine radio system.")
   (database :initform nil
             :initarg :database
             :accessor web-radio-database
             :documentation "The music database."))
  (:documentation "Web radio for Ernestine."))


;; ------------
;; Web Service
;; ------------


(defun make-web-page (path &rest variables)
  (let ((common (list :title "Ernestine radio"
                      :version *version*)))
    (with-output-to-string (stream)
      (html-template:fill-and-print-template path
                                             (append common (car variables))
                                             :stream stream))))


(defun get-playlist-name (&key artist-name album-name genre-name)
  "Creates the playlist filename using ARTIST-NAME and ALBUM-NAME."
  (let ((playlist
         (cond ((and artist-name album-name
                     (string-not-equal "" artist-name)
                     (string-not-equal "" album-name))
                (cl-ppcre:regex-replace-all " "
                                            (format nil "playlist-~A-~A.xspf"
                                                    artist-name album-name)
                                            "_"))
               ((and artist-name (string-not-equal "" artist-name))
                (cl-ppcre:regex-replace-all " "
                                            (format nil "playlist-~A.xspf"
                                                    artist-name)
                                            "_"))
               ((and genre-name (string-not-equal "" genre-name))
                (cl-ppcre:regex-replace-all " "
                                            (format nil "playlist-genre-~A.xspf"
                                                    genre-name)
                                            "_"))
               (t +playlist-name+))))
    playlist))
        


(defgeneric get-music-uri (web-radio)
  (:documentation "Get the URI to access to music file."))


(defmethod get-music-uri ((web-radio web-radio))
  (with-slots (host port) web-radio
    (concatenate 'string
                 "http://"
                 host ":" (format nil "~A" port)
                 "/ernestine-radio/music/")))


(defun add-song-to-xspf (xspf uri song &optional image)
  "Convert an Ernestine song to a Track."
  (cl-xspf:add-track xspf
                     (concatenate 'string uri (song-filename song))
                     (song-title song)
                     (song-artist song)
                     (song-album song)
                     :image image))


(defgeneric creates-playlist (web-radio &key artist-name album-name)
  (:documentation "Creates a XSPF playlist from music database.
If ARTIST-NAME is specified creates playlist for only ARTIST-NAME's albums.
If ALBUM-NAME is specified creates playlist for only this album"))


(defmethod creates-playlist ((web-radio web-radio)
                             &key artist-name album-name genre-name)
  (with-slots (database host port xspf) web-radio
    (let ((uri (get-music-uri web-radio)))
      (cl-xspf:add-playlist xspf :creator "Ernestine")
      (labels ((music-to-xspf (db art-name alb-name image)
                 (do-songs (song) db art-name alb-name
                   (add-song-to-xspf xspf uri song image))))
        (cond ((and artist-name album-name
                    (string-not-equal "" artist-name)
                    (string-not-equal "" album-name))
               (when *debug*
                 (format t "~&Treat artist ~A ~A" artist-name album-name))
               (let ((album (get-artist-album database artist-name album-name)))
                 (music-to-xspf database artist-name album-name (album-cover album))))
              ((and artist-name
                    (string-not-equal "" artist-name))
               (when *debug*
                 (format t "~&Treat artist ~A" artist-name))
               (do-albums (album) database artist-name
                 (music-to-xspf database artist-name (album-name album) (album-cover album))))
              ((and genre-name
                    (string-not-equal "" genre-name))
               (let ((songs (get-music-by-genre database (list genre-name))))
                 (loop for song in songs
                    do (add-song-to-xspf xspf uri song))))
              (t
               (do-artists (artist) database
                 (when *debug*
                   (format t "~&Treat artist ~A" (artist-name artist)))
                 (do-albums (album) database (artist-name artist)
                   (music-to-xspf database (artist-name artist) (album-name album) (album-cover album))))))))))


(defgeneric generate-playlist (web-radio &key filename artist-name album-name genre-name)
  (:documentation "Print to FILENAME a playlist in XSPF format."))


(defmethod generate-playlist ((web-radio web-radio)
                              &key (filename +playlist-name+)
                              artist-name album-name genre-name)
  (let ((file (concatenate 'string
                           *playlist-directory*
                           filename)))
    (creates-playlist web-radio
                      :artist-name artist-name
                      :album-name album-name
                      :genre-name genre-name)
    (cl-xspf:write-playlist (web-radio-xspf web-radio) file)))


(defgeneric add-database (web-radio music-directory)
  (:documentation "Add to the music database contents of DIRECTORY."))


(defmethod add-to-database ((web-radio web-radio) music-directory
                            &optional covers-p)
  (with-slots (database) web-radio
    (scan-directory database music-directory :covers-p covers-p)))


(defgeneric add-music (web-radio music-directory &optional covers-p)
  (:documentation "Add music files to the database and regenerate the
XSPF playlist. If COVERS-P is T, search albums covers."))


(defmethod add-music ((web-radio web-radio) music-directory
                      &optional covers-p)
  (add-to-database web-radio music-directory covers-p)
  (generate-playlist web-radio))


(defgeneric update-database (web-radio)
  (:documentation "Search for albums' covers and update the music database."))


(defmethod update-database ((web-radio web-radio))
  (with-slots (database) web-radio
    (do-artists (artist) database
      (do-albums (album) database (artist-name artist)
        (let ((covers
               (cl-covers:search-album-covers (artist-name artist) (album-name album))))
          (when *debug*
            (format t "~&Cover ~A ~A : ~A"
                    (artist-name artist) (album-name album) covers))
          (setf (album-cover album) (first covers)))))))



(defgeneric handle-index (web-radio)
  (:documentation "Print main page."))


(defmethod handle-index ((web-radio web-radio))
  (with-slots (database) web-radio
    (hunchentoot:log-message :info "Index")
    (let ((artists '())
          (albums '())
          (genres '())
          (art-name (hunchentoot:get-parameter "artist_name"))
          (alb-name (hunchentoot:get-parameter "album_name"))
          (genre-name (hunchentoot:get-parameter "genre_name")))
      (do-artists (artist) database
        (let ((selected (if (string-equal (artist-name artist) art-name)
                            "selected" "")))
          (setf artists
                (append artists
                        (list (list :name (artist-name artist)
                                    :selected selected))))))
      (unless (and genre-name (string-not-equal "" genre-name))
        (when (and art-name (string-not-equal "" art-name))
          (do-albums (album) database art-name
            (let ((selected (if (string-equal (album-name album) alb-name)
                                "selected" "")))
              (setf albums
                    (append albums
                            (list (list :name (album-name album)
                                        :selected selected))))))))
      (let ((music-genres (get-genre database)))
        (loop for genre in music-genres
           as selected = (if (string-equal genre genre-name)
                             "selected"
                             "")
           do (setf genres
                    (append genres
                            (list (list :name genre
                                        :selected selected))))))
      (let ((playlist
             (get-playlist-name :artist-name art-name
                                :album-name alb-name
                                :genre-name genre-name)))
        (hunchentoot:log-message :info "Index ~A - ~A :: ~A :: ~A = ~A"
                                 art-name alb-name artists albums playlist)
        (generate-playlist web-radio
                           :filename playlist
                           :artist-name art-name
                           :album-name alb-name
                           :genre-name genre-name)
        (make-web-page #p"index.html" (list :playlist playlist
                                            :artists artists
                                            :albums albums
                                            :genres genres))))))


(defgeneric start-radio (web-radio)
  (:documentation "Start the Ernestine web radio."))


(defmethod start-radio ((web-radio web-radio))
  (with-slots (port main-dir listener) web-radio
    (let ((html-dir (concatenate 'string main-dir +templates-directory+)))
      (setf html-template:*default-template-pathname* html-dir
            hunchentoot:*dispatch-table*
            (nconc
             (mapcar (lambda (args)
                       (apply #'hunchentoot:create-prefix-dispatcher args))
                     (list (list "/ernestine-radio/index"
                                 #'(lambda ()
                                     (handle-index web-radio)))))
             (list (hunchentoot:create-folder-dispatcher-and-handler
                    "/ernestine-radio/css/" 
                    (make-pathname :defaults (concatenate 'string
                                                          main-dir
                                                          +stylesheets-directory+))))
             (list (hunchentoot:create-folder-dispatcher-and-handler
                    "/ernestine-radio/img/" 
                    (make-pathname :defaults (concatenate 'string
                                                          main-dir
                                                          +images-directory+))))
             (list (hunchentoot:create-folder-dispatcher-and-handler
                    "/ernestine-radio/html/" 
                    (make-pathname :defaults (concatenate 'string
                                                          main-dir
                                                          +templates-directory+))))
             (list (hunchentoot:create-folder-dispatcher-and-handler
                    "/ernestine-radio/music/" "/")) ;*music-directory*))
             (list (hunchentoot:create-folder-dispatcher-and-handler
                    "/ernestine-radio/playlist/" *playlist-directory*))
             (list #'hunchentoot:default-dispatcher))
            listener (hunchentoot:start-server :port port)))))


(defgeneric stop-radio (web-radio)
  (:documentation "Stop the Ernestine web radio."))


(defmethod stop-radio ((web-radio web-radio))
  (with-slots (listener) web-radio
    (hunchentoot:stop-server listener)))


(defun create-web-radio (host &key (port *port*) covers-p)
  "Creates a new web radio on HOST and creates the playlist from the
music database.
PORT is the port the server will be listening on (the default is 9090).
If COVERS-P is T, search albums covers and update music database."
  (let* ((home (get-ernestine-directory))
         (db (ernestine-database:get-backend :prevalence home))
         (radio (make-instance 'web-radio
                               :host host
                               :port port
                               :xspf (make-instance 'cl-xspf:xspf)
                               :main-dir *ernestine-directory*
                               :database db)))
    (when covers-p
      (update-database radio))
    (generate-playlist radio)
    radio))


(defun debug-mode (&optional debugp)
  "Set or reset the debug mode."
  (setf *debug* debugp
        hunchentoot:*show-access-log-messages* debugp
        hunchentoot:*show-lisp-backtraces-p* debugp
        hunchentoot:*show-lisp-errors-p* debugp
        hunchentoot:*log-lisp-backtraces-p* debugp
        hunchentoot:*log-lisp-errors-p* debugp
        hunchentoot:*log-lisp-warnings-p* debugp))