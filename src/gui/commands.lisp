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


;; -----------------
;; Music management
;; -----------------


(define-player-command (com-start-song :name t :menu nil
                                               :keystroke (#\Return :control))
    ()
  (play-song *ernestine-player*))


(define-player-command (com-stop-song :name t :menu nil
                                              :keystroke (#\Space :control))

    ()
  (stop-song *ernestine-player*)
  (setf (slot-value *ernestine-player* 'continue) nil))


(define-player-command (com-previous-song :name t :menu nil
                                                  :keystroke (#\p :control))
    ()
  (with-rank (rank) *ernestine-player*
    (let ((next (1- rank)))
      (when (>= next 0)
        (another-song *ernestine-player* next)))))


(define-player-command (com-next-song :name t :menu nil
                                              :keystroke (#\n :control))
    ()
  (with-rank (rank) *ernestine-player*
    (let ((next (1+ rank)))
      (when (> next 0)
        (another-song *ernestine-player* next)))))

                       
(define-player-command (com-quit :name t :menu nil :keystroke (#\q :control))
    ()
   (clim:frame-exit *ernestine-player*))


(define-player-command (com-add-playlist :name t :menu nil
                                                 :keystroke (#\a :control))
    ()
  (with-slots (database playlist artist album song) *ernestine-player*
    (cond ((and song (not (find song playlist)))
           (push song playlist))
          (album
           (do-songs (current-song) database (artist-name artist) (album-name album)
             (unless (find current-song playlist)
               (push current-song playlist))))
          (artist
           (do-albums (current-album) database (artist-name artist)
             (do-songs (current-song) database (artist-name artist) (album-name current-album)
               (unless (find current-album playlist)
                 (push current-song playlist))))))))


(define-player-command (com-del-playlist :name t :menu nil
                                                 :keystroke (#\d :control))
    ()
  (with-slots (playlist song) *ernestine-player*
    (when song
      (setf playlist (delete song playlist)))))


(define-player-command (com-shuffle-playlist :name t :menu nil
                                                     :keystroke (#\z :control))
    ()
  (with-slots (shuffle-p) *ernestine-player*
    (setf shuffle-p (not shuffle-p))))


(define-player-command (com-scan-directory :name t :menu nil
                                                   :keystroke (#\i :control))
    ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'scan
                                :height 430
                                :width 350
                                :calling-frame *ernestine-player*
                                :doc-title "Ernestine - Search music")))


(define-player-command (com-help :name t :menu nil :keystroke (#\h :control))
    ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'help
                                :height 330
                                :width 400
                                :calling-frame *ernestine-player*
                                :doc-title "Ernestine - Help")))


(define-player-command (com-about :name t :menu nil)
    ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'about
                                :height 230
                                :width 450
                                :calling-frame *ernestine-player*
                                :doc-title "Ernestine About box")))


;; -------
;; Layout
;; -------


(define-player-command (com-minimal :name t :menu nil :keystroke (#\m :control))
    ()
  (setf (clim:frame-current-layout clim:*application-frame*) 'minimal)
  (clim:change-space-requirements (clim:frame-panes clim:*application-frame*)
                                  :resize-frame t))


(define-player-command (com-default :name t :menu nil
                                            :keystroke (#\m :control :shift))
    ()
  (setf (clim:frame-current-layout clim:*application-frame*) 'default)
  (clim:change-space-requirements (clim:frame-panes clim:*application-frame*)
                                  :resize-frame t))


(define-player-command (com-tab :name t :menu nil
                                        :keystroke (#\t :control))
    ()
  (setf (clim:frame-current-layout clim:*application-frame*) 'no-tab)
  (clim:change-space-requirements (clim:frame-panes clim:*application-frame*)
                                  :resize-frame t))


;; ----------
;; Selection
;; ----------


(define-player-command com-select-artist
    ((artist 'artist :gesture :select))
  (setf (slot-value *ernestine-player* 'artist) artist
        (slot-value *ernestine-player* 'album) nil)
  (redisplay-callback *ernestine-player* 'albums))


(define-player-command com-select-album
    ((album 'album :gesture :select))
  (setf (slot-value *ernestine-player* 'album) album)
  (redisplay-callback *ernestine-player* 'songs))


(define-player-command com-select-song
    ((song 'song :gesture :select))
  (let ((current (slot-value *ernestine-player* 'song)))
    (if (and current
             (string-equal (song-title current)
                           (song-title song)))
        (progn
          (setf (slot-value *ernestine-player* 'song) song)
          (play-song *ernestine-player*))
        (setf (slot-value *ernestine-player* 'song) song))))
        

;; ---------
;; Playlist
;; ---------


(define-player-command (com-save-playlist :name t :menu nil
                                                  :keystroke (#\s :control))
    ()
  (let* ((page (clim-tab-layout:tab-layout-enabled-page (tab-ernestine-layout))))
    (when page
      (let* ((title (clim-tab-layout:tab-page-title page))
             (filename (if (string-not-equal "Playlist" title)
                           (format nil "~A~A.xspf"
                                   (get-ernestine-directory)
                                   title)
                           (let* ((filter (format nil "~A" (gensym)))
                                  (dir
                                   (clim-utils:file-selector-pane *ernestine-player*
                                                                  :current (get-ernestine-directory)
                                                                  :filters (list filter))))
                             (format nil "~A~A-~A.xspf" dir title (iso-time)))))
             (xspf (make-instance 'cl-xspf:xspf)))
        (cl-xspf:add-playlist xspf :creator "Ernestine")
        (let ((playlist (find-playlist *ernestine-player*)))
          (loop for song in playlist
             do (cl-xspf:add-track xspf
                                   (song-filename song)
                                   (song-title song)
                                   (song-artist song) 
                                   (song-album song))))
        (cl-xspf:write-playlist xspf filename)))))


 (define-player-command (com-open-playlist :name t :menu t
                                                   :keystroke (#\o :control))
     ()
   (let ((file (clim-utils:file-selector-pane *ernestine-player*
                                               :current (get-ernestine-directory)
                                               :filters '("xspf"))))
     (when (and file
                (not (cl-fad:directory-exists-p file)))
       (let ((fm (clim:frame-manager *ernestine-player*))
             (name (pathname-name file)))
         (clim:with-look-and-feel-realization (fm *ernestine-player*)
           (let ((page 
                  (make-instance 'clim-tab-layout:tab-page
                                 :title name
                                 :pane (clim:scrolling ()
                                         (clim:make-pane 'clim:application-pane
                                                         :incremental-redisplay t
                                                         :scroll-bars t
                                                         :display-function #'display-dynamic-playlist)))))
             (setf (clim-tab-layout:tab-layout-pages (tab-ernestine-layout))
                   (append (clim-tab-layout:tab-layout-pages (tab-ernestine-layout))
                           (list page))
                   (clim-tab-layout:tab-layout-enabled-page (tab-ernestine-layout))
                   page)))
         (let ((xspf (make-instance 'cl-xspf:xspf)))
           (cl-xspf:read-playlist xspf file)
           (with-slots (dynamic-playlists) *ernestine-player*
             (push (cons name
                         (loop for track in (cl-xspf:get-tracks xspf)
                            collect (make-instance 'song
                                                   :title (cl-xspf:title track)
                                                   :album (cl-xspf:album track)
                                                   :artist (cl-xspf:creator track)
                                                   :rank ""
                                                   :time ""
                                                   :filename (cl-xspf:location track))))
                   dynamic-playlists)))))))


(define-player-command (com-new-playlist :name t)
    ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'playlist
                                :height 230
                                :width 250
                                :calling-frame *ernestine-player*
                                :doc-title "Ernestine Dynamic Playlist")))

