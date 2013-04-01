;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          gui.lisp
;;;; Purpose:       Ernestine GUI Player
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



(defun start-player (backend)
  "Create the Ernestine player.
BACKEND contains the music database"
  #+nil
  (loop for port in climi::*all-ports*
     do (climi::destroy-port port))
  (setf clim:*default-text-style* (clim:make-text-style
                                   :sans-serif :roman :normal)
        *ernestine-player* (clim:make-application-frame 'player
                                                        :database backend
                                                        :height 865
                                                        :width 800
                                                        :doc-title "Ernestine player")
        *music-bigb* (sb-thread:make-thread (lambda ()
                                              (loop-music *ernestine-player*))
                                            :name "Music Bigb"))
  (clim:run-frame-top-level *ernestine-player*))


(defun player (&optional last-fm-account)
  "Load Ernestine music player.
LAST-FM-ACCOUNT is a (login . password) informations about a Last.fm account."
  (when (check-installation)
    (let ((home (get-ernestine-directory)))
      (init-log home)
      (when last-fm-account
        (setf *last-fm*
              (cl-audioscrobbler:make-client (car last-fm-account)
                                             (cdr last-fm-account)
                                             +cl-audioscrobbler-client-id+
                                             +cl-audioscrobbler-client-version+)))
      (cl-log:log-message :info "[Start] Ernestine ...~%")
      (start-player (ernestine-database:get-backend :prevalence home))))
    0) ;; for sb-executable


;; ------
;; Tools
;; ------


(defun find-playlist (player)
  "Find the current playlist : default or dynamic."
  (let* ((page (clim-tab-layout:tab-layout-enabled-page (tab-ernestine-layout))))
    (if (string-equal "Playlist" (clim-tab-layout:tab-page-title page))
        (player-playlist player)
        (reverse (cdr (find (clim-tab-layout:tab-page-title page)
                            (player-dynamic-playlist player)
                            :test #'string-equal
                            :key #'car))))))


(defmacro with-rank ((rank) player &body body)
  "Macro which find RANK of the current song played by PLAYER,
and executes BODY.
It could be a song from selected album, or from playlists."
  `(with-slots (song) ,player
     (let ((playlist (find-playlist ,player)))
       (when (and ,player song)
         (let* ((songs (reverse playlist))
                (playlist-rank (position (song-title song) songs
                                         :test #'string-equal
                                         :key #'song-title)))
           (if playlist-rank
               (let ((,rank playlist-rank))
                 ,@body)
               (when song
                 (let ((,rank (parse-integer (song-rank song))))
                   ,@body))))))))


(defun play-song (player)
  "Plays song FILENAME, and updates the 'info pane of
the main player."
  (with-slots (process artist album song current continue last-fm-delay) player
    (when process
      (sb-ext:process-kill process 9)
      (setf last-fm-delay 0))
    (when song
      (cl-log:log-message :info "[Player] Play song ~A~%"
                          (song-title song))
      (setf current song
            process (sb-ext:run-program *player-command*
                                        (append (list (song-filename current))
                                                +player-args+)
                                        :wait nil)))
    (setf continue t)
    (redisplay-callback player 'songs)
    (redisplay-callback player 'info)))


(defun stop-song (frame)
  "Stop playing current song."
  (with-slots (current process continue last-fm-delay) frame
    (when process
      (sb-ext:process-kill process 9))
    (setf current nil
          continue nil
          last-fm-delay 0)))


(defun another-song (player rank)
  "Select another song using his RANK, if it exists, play it."
  (with-slots (database artist album song shuffle-p) player
    (let* ((playlist (find-playlist player))
           (songs (reverse playlist))
           (next-song (if (position (song-title song) songs
                                    :test #'string-equal
                                    :key #'song-title)
                          (if shuffle-p
                              (nth (random (1- (length songs))) songs)
                              (nth rank songs))
                          (get-album-song-by-rank database
                                                  (artist-name artist)
                                                  (album-name album)
                                                  (format nil "~A" rank)))))
      (when next-song
        (when *debug*
          (cl-log:log-message :info
                              "Next song ~A~%" (song-title next-song)))
        (stop-song player)
        (setf song next-song)
        (play-song player)))))


(defun loop-music (player)
  "Loop which examines process."
  (when *debug* (format t "Music process management"))
  (with-slots (process artist album song continue last-fm-delay) player
    (loop as timer = 0.1
       do
         (sleep timer)
         (setf last-fm-delay (+ last-fm-delay timer))
         (unless process
           (setf last-fm-delay 0))
         (when (and process continue)
;;            (when *debug*
;;              (format t "Loop music ....~%"))
           (when (and *last-fm*
                      (sb-ext:process-alive-p process)
                      (= 10.0 (/ (floor (* last-fm-delay 10)) 10.0)))
             (send-last-fm artist album song))
           (unless (and (sb-ext:process-alive-p process)
                        song)
             (with-rank (rank) player
               (setf last-fm-delay 0)
               (another-song player (+ rank 1))))))))


(defun redisplay-callback (app pane)
  "Force APP to redisplay PANE."
  (clim:redisplay-frame-pane app
                             (clim:get-frame-pane app pane)
                             :force-p t))


(defun adjust-volume (level)
  "Update the volume to LEVEL."
  (sb-ext:run-program *volume-command*
                      (append (list +volume-args+ level))
                      :wait nil))


(defun hilight-line (pane y)
  "Higligth with *hilight-color* a line"
  (multiple-value-bind (pane-x1 pane-y1 pane-x2 pane-y2)
      (clim:bounding-rectangle* pane)
    (let ((height (clim:text-style-height clim:*default-text-style* pane)))
      (clim:draw-rectangle* pane
                            pane-x1 y pane-x2 (+ y height 1)
                            :filled t :ink *hilight-color*))))


;; ----------
;; Callbacks
;; ----------


(defun play-callback (button)
  "Callback to start playing selected song."
  (let ((frame (clim:gadget-client button)))
    (play-song frame)))


(defun stop-callback (button)
  "Callback for stopping to play current song"
  (let ((frame (clim:gadget-client button)))
    (stop-song frame)
    (setf (slot-value frame 'continue) nil)))


(defun next-callback (button)
  "Callback to stop playing current song, selects the
next song on the artist's album, and starts to play it."
  (let ((frame (clim:gadget-client button)))
    (with-rank (rank) frame
      (another-song *ernestine-player* (+ rank 1)))))


(defun previous-callback (button)
  "Callback to stop playing current song, selects the
previous song on the artist's album, and starts to play it."
  (let ((frame (clim:gadget-client button)))
    (with-rank (rank) frame
      (let ((next (1- rank)))
        (when (> next 0)
          (another-song *ernestine-player* next))))))


(defun adjust-volume-callback (slider value)
  "Callback for managing sound volume"
  (declare (ignore slider))
  (adjust-volume (format nil "~A" (round (float value)))))



(defun add-callback (button)
  "Callback to display the music management box."
  (clim:run-frame-top-level
   (clim:make-application-frame 'scan
                                :height 430
                                :width 350
                                :calling-frame (clim:gadget-client button)
                                :doc-title "Ernestine - Search music")))


;; -----
;; GUI
;; ----


(clim:make-command-table 'main-menu
                         :errorp nil
                         :menu '(("Search music" :command com-scan-directory)
                                 ("Help" :command com-help)
                                 ("About" :command com-about)
                                 ("Quit" :command com-quit)))


(clim:make-command-table 'playlist-menu
                         :errorp nil
                         :menu '(("Save a playlist" :command com-save-playlist)
                                 ("Load a playlist" :command com-open-playlist)
                                 ("New dynamic playlist" :command com-new-playlist)))


(clim:define-application-frame player
    (clim:standard-application-frame redisplay-frame-mixin)
  ((database :initform '()
             :initarg :database
             :accessor player-database
             :documentation "Music database.")
   (artist :initform nil
           :initarg :artist
           :accessor player-artist
           :documentation "Current artist.")
   (album :initform nil
           :initarg :album
           :accessor player-album
           :documentation "Current album.")
   (song :initform nil
         :initarg :song
         :accessor player-song
         :documentation "Selected song.")
   (current :initform nil
            :initarg :current
            :accessor player-current
            :documentation "Current song played.")
   (playlist :initform '()
             :initarg :playlist
             :accessor player-playlist
             :documentation "A list of selected songs.")
   (dynamic-playlists :initform '()
                      :initarg :dynamic-playlist
                      :accessor player-dynamic-playlist
                      :documentation "A list of playlists.")
   (shuffle-p :initform nil
              :initarg :shuffle-p
              :accessor player-shuffle-p
              :documentation "Ordered playlist or not.")
   (process :initform nil
            :initarg :process
            :accessor player-process
            :documentation "Process which plays current song")
   (continue :initform nil
             :initarg :continue
             :accessor player-continue
             :documentation "Flag to know if the player must read all songs.")
   (last-fm-delay :initform 0
                  :initarg :last-fm-delay
                  :accessor player-last-fm-delay
                  :documentation "Delay to send informations to Last.fm."))
  (:menu-bar (("Ernestine" :menu main-menu)
              ("Playlist" :menu playlist-menu)))
  (:panes (artists :application
                   :min-width 250
                   :width 250
                   :incremental-redisplay t
                   :display-function #'display-artists)
          (albums :application
                  :incremental-redisplay t
                  :display-function #'display-albums)
          (songs :application
                 :incremental-redisplay t
                 :display-function #'display-songs)
          (selection :application
                     :incremental-redisplay t
                     :display-function #'display-playlist)
          (info :application
                :scroll-bars nil
                :background clim:+black+
                :foreground clim:+white+
                :min-height 30
                :incremental-redisplay t
                :display-function #'display-info)
          ;;(help-bt (make-button "Help" #'help-callback))
          ;;(about-bt (make-button "About" #'about-callback))
          (play-bt (make-button "Play" #'play-callback))
          (previous-bt (make-button "Previous" #'previous-callback))
          (next-bt (make-button "Next" #'next-callback))
          (stop-bt (make-button "Stop" #'stop-callback))
          ;;(add-bt (make-button "Music" #'add-callback))
          (adjust-first (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (adjust-second (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (adjust-third (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (slider-volume :slider
                         :drag-callback nil
                         :value-changed-callback #'adjust-volume-callback
                         :min-value 0
                         :max-value 100
                         :value 0
                         :show-value-p t
                         :orientation :horizontal
                         :width 120))
  (:layouts
   (default
    (clim:vertically (:equalize-width nil :equalize-height nil
                         :height 500 :width 200)
      (clim:horizontally (:equalize-height nil)
        clim:+fill+
        (clim:labelling (:label "Ernestine"
                                :text-style (clim:make-text-style :sans-serif
                                                                  :roman
                                                                  :huge)
                                :align-x :center
                                :align-y :center))
        clim:+fill+
        slider-volume
        clim:+fill+
        (clim:labelling (:label "Control Panel")
          (clim:horizontally ()
            stop-bt previous-bt play-bt next-bt)))
      (clim-tab-layout:with-tab-layout ('clim-tab-layout:tab-page
                                        :name 'ernestine-tab)
        ("Music" (clim:horizontally ()
                   (clim:labelling (:label "Artists")
                     artists)
                   adjust-first
                   (clim:vertically ()
                     (clim:labelling (:label "Albums")
                       albums)
                     adjust-second
                     (clim:labelling (:label "Songs")
                       songs))))
        ("Playlist" selection))
      info))
   (no-tab
       (clim:vertically (:equalize-width nil :equalize-height nil
                         :height 500 :width 200)
         (clim:horizontally (:equalize-height nil)
                clim:+fill+
                (clim:labelling (:label "Ernestine"
                                        :text-style (clim:make-text-style :sans-serif
                                                                          :roman
                                                                          :huge)
                                        :align-x :center
                                        :align-y :center))
                clim:+fill+
                slider-volume
                clim:+fill+
                (clim:labelling (:label "Control Panel")
                  (clim:horizontally ()
                    stop-bt previous-bt play-bt next-bt)))
         (clim:horizontally ()
                (clim:labelling (:label "Artists")
                  artists)
                adjust-first
                (clim:vertically ()
                  (clim:labelling (:label "Albums")
                    albums)
                  adjust-second
                  (clim:labelling (:label "Songs")
                    songs)))
         adjust-third
         (clim:labelling (:label "Playlist")
           selection)        
         info))
   (minimal
    (clim:vertically (:equalize-width nil :equalize-height nil)
      (clim:horizontally (:equalize-height nil)
        clim:+fill+
        (clim:labelling (:label "Ernestine"
                                   :text-style (clim:make-text-style :sans-serif
                                                                     :roman
                                                                     :huge)
                                   :align-x :center
                                   :align-y :center))
        clim:+fill+
        slider-volume
        clim:+fill+
        (clim:labelling (:label "Control Panel")
          (clim:horizontally ()
            stop-bt previous-bt play-bt next-bt)))
      info))
   ))


(defmethod clim:frame-exit :before ((frame player))
  (cl-log:log-message :info "[Stop] Ernestine ...~%")
  (when (player-process frame)
    (sb-ext:process-kill (player-process frame) 9))
  (when (sb-thread:thread-alive-p *music-bigb*)
    (sb-thread:terminate-thread *music-bigb*)))


(defun sane-find-pane-named (frame name)
  (find name
	(climi::frame-named-panes frame)
	:key #'clim:pane-name
	:test #'string-equal))


(defun ernestine-tab ()
  (sane-find-pane-named clim:*application-frame*
                        'ernestine-tab))


;; ------
;; Views
;; ------


(defclass info-view (clim:view)
  ())


(defclass playlist-view (clim:view)
  ())




;; --------------
;; Presentations
;; --------------


(clim:define-presentation-type artist ())

(clim:define-presentation-method clim:present (artist (type artist) stream view &key args)
  (declare (ignore view))
  (format stream "~A~%" (artist-name artist)))


(clim:define-presentation-type album ())

(clim:define-presentation-method clim:present (album (type album) stream view &key args)
  (declare (ignore view))
  (format stream "~A~%" (album-name album)))


(clim:define-presentation-type song ())

(clim:define-presentation-method clim:present (song (type song) stream view &key args)
  (declare (ignore view))
  (format stream "~A~%" (song-title song)))

(clim:define-presentation-method clim:present (song (type song) stream (view info-view) &key)
  (declare (ignore view))
  (format stream "~A ~A [ ~A ] ~A"
          (song-rank song)
          (song-title song)
          (song-album song)
          (song-time song)))

(clim:define-presentation-method clim:present (song (type song) stream (view playlist-view) &key)
  (declare (ignore view))
  (clim:formatting-row (stream)
    (clim:formatting-cell (stream :align-x :right)
      (format stream "~A" (song-title song)))
    (clim:formatting-cell (stream :align-x :right)
      (format stream "~A" (song-album song)))
    (clim:formatting-cell (stream :align-x :right)
      (format stream "~A" (song-artist song)))
    (clim:formatting-cell (stream :align-x :right)
      (format stream "~A" (song-time song)))))


;; --------
;; Display 
;; --------


(defmacro with-current-data ((current elem fct) &body body)
  "Macro which verify if CURRENT and ELEM are equal by FCT predicate,
and uses this results for executes BODY which present ELEM."
  `(if (and ,current
            (string-equal (funcall ,fct ,elem)
                          (funcall ,fct ,current)))
       (clim:with-drawing-options
           (stream
            :text-style (clim:make-text-style
                         :sans-serif
                         :bold
                         :normal))
         ,@body)
       ,@body))


(defgeneric display-artists (frame stream)
  (:documentation "Display artists."))


(defmethod display-artists ((frame player) stream)
  (let ((current-artist (player-artist frame)))
    (do-artists (artist) (player-database frame)
      (clim:updating-output (stream :unique-id artist)
        (with-current-data (current-artist artist #'artist-name)
          (clim:present artist 'artist :stream stream))))
    (terpri stream)))


(defgeneric display-albums (frame stream)
  (:documentation "Display album of a selected artist."))


(defmethod display-albums ((frame player) stream)
  (let ((artist (player-artist frame)))
    (when artist
      (let ((name (artist-name artist))
            (backend (player-database frame))
            (current-album (player-album frame)))
        (do-albums (album) backend name
          (clim:updating-output (stream :unique-id album)
            (with-current-data (current-album album #'album-name)
              (clim:present album 'album :stream stream))))))
    (terpri stream)))


(defgeneric display-songs (frame stream)
  (:documentation "Display songs of a selected album."))


(defmethod display-songs ((frame player) stream)
  (let ((artist (player-artist frame))
        (album (player-album frame)))
    (when (and artist album)
      (let ((artistname (artist-name artist))
            (albumname (album-name album))
            (backend (player-database frame))
            (current-song (player-song frame)))
        (do-songs (song) backend artistname albumname
          (clim:updating-output (stream :unique-id song)
            (with-current-data (current-song song #'song-title)
              (clim:present song 'song :stream stream))))))
    (terpri stream)))


(defgeneric display-info (frame stream)
  (:documentation "Display information about current song"))

(defmethod display-info ((frame player) stream)
  (with-slots (process current continue) frame
    (when (and process continue current)
      (format stream "~A ~A [ ~A ], ~A ~A"
              (song-rank current)
              (song-title current)
              (song-album current)
              (song-artist current)
              (song-time current))))
  (terpri stream))


(defgeneric display-playlist (frame stream)
  (:documentation "Display selected songs"))


(defmethod display-playlist ((frame player) stream)
  (let ((playlist (reverse (player-playlist frame)))
        (current-song (player-song frame)))
    (clim:formatting-table (stream :x-spacing '(3 :character)
                                   :y-spacing '(1 :character))
      (clim:formatting-row (stream)
        (clim:with-text-style (stream '(:sans-serif :bold nil))
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Title"))
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Album"))
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Artist"))
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Time"))))
      (loop for song in playlist
         do (with-current-data (current-song song #'song-title)
              (clim:present song 'song
                            :stream stream
                            :view (make-instance 'playlist-view)))))))


(defgeneric display-dynamic-playlist (frame stream)
  (:documentation "Display songs selected by a dynamic playlist."))


(defmethod display-dynamic-playlist ((frame player) stream)
  (let ((page (clim-tab-layout:tab-layout-enabled-page (tab-ernestine-layout)))
        (current-song (player-song frame)))
    (when page
      (clim:formatting-table (stream :x-spacing '(3 :character)
                                     :y-spacing '(1 :character))
        (clim:formatting-row (stream)
          (clim:with-text-style (stream '(:sans-serif :bold nil))
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Title"))
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Album"))
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Artist"))
            (clim:formatting-cell (stream :align-x :right)
              (format stream "Time"))))
        (loop for song in (cdr (find (clim-tab-layout:tab-page-title page)
                                     (player-dynamic-playlist *ernestine-player*)
                                     :test #'string-equal
                                     :key #'car))
           do (with-current-data (current-song song #'song-title)
                (clim:present song 'song
                              :stream stream
                              :view (make-instance 'playlist-view))))))))
                              
          

