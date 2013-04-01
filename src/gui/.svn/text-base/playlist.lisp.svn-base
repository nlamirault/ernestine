;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          playlist.lisp
;;;; Purpose:       Ernestine Playlist management.
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

;; ---------------------------
;; Stolen from McClim tabdemo

(defun sane-find-pane-named (frame name)
  (find name
        (climi::frame-named-panes frame)
        :key #'clim:pane-name
        :test #'string-equal))

(defun tab-ernestine-layout ()
  (sane-find-pane-named *ernestine-player* 'ernestine-tab))


;; End
;; ----


(defun create-playlist (button)
  "Creates a new dynamic playlist."
  (let* ((frame (clim:gadget-client button))
         (genres
          (mapcar #'clim:gadget-id
                  (clim:gadget-value
                   (clim:find-pane-named frame 'style-pane))))
         (fm (clim:frame-manager frame))
         (name (clim:gadget-value
                   (clim:find-pane-named frame 'title-pane))))
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
    (with-slots (database dynamic-playlists) *ernestine-player*
      (let ((songs (get-music-by-genre database genres)))
        (when songs
          (push (cons name songs) dynamic-playlists))))
    (clim:frame-exit frame)))


(clim:define-application-frame playlist
    (clim:standard-application-frame redisplay-frame-mixin)
  ()
  (:panes
   (style-pane
    (clim:with-radio-box (:type :some-of)
      (let ((genres (get-genre (player-database *ernestine-player*))))
        (loop for style in genres
           do (clim:make-pane 'clim:toggle-button
                              :label style
                              :id style)))))
   (title-pane :text-field :value "")
   (ok-bt (make-button "Ok" 'create-playlist)))
  (:layouts
   (default
       (clim:vertically ()
         (clim:labelling (:label "Music style")
           (clim:scrolling ()
             style-pane))
         (clim:labelling (:label "Playlist title")
           title-pane)
         ok-bt))))


