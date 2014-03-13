;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools.lisp
;;;; Purpose:       Some McClim tools
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


;; -----------------------------------------------------------------
;; Hack for scroll pane and formatting table
;; From : common-lisp.net/~rtoy/maxima-repl-2005-07-02.lisp

(defclass redisplay-frame-mixin ()
  ())

(defmethod clim:redisplay-frame-pane :after
    ((frame redisplay-frame-mixin) (pane clim:application-pane) &key force-p)
  (declare (ignore force-p))
  (clim:change-space-requirements
   pane
   :height (clim:bounding-rectangle-height (clim:stream-output-history pane))))

;; Hack end
;; -----------------------------------------------------------------



(defun make-button (label callback &key width height
                    (max-width clim:+fill+) min-width
                    (max-height clim:+fill+) min-height)
  "Creates a new button."
  (clim:make-pane 'clim:push-button
                  :label label
                  :activate-callback callback
                  :width width :height height
                  :max-width  max-width :min-width min-width
                  :max-height max-height :min-height min-height))


(defun close-box-callback (button)
  "Callback to close dialog box."
  (finish-output *error-output*)
  (clim:frame-exit (clim:gadget-client button)))


;; Hack based on iso-time from Hunchentoot
(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d"
            year month date hour minute second)))

(defun send-last-fm (artist album song)
  "Send SONG's informations to LAST.FM."
  (cl-log:log-message :info "[Last.FM] ~A~%"
                      ;;(artist-name artist)
                      ;;(album-name album)
                      (song-title song))
  ;; (handler-case
  ;;     (cl-audioscrobbler:post-song *last-fm*
  ;;                                  :1.1
  ;;                                  (artist-name artist)
  ;;                                  (album-name album)
  ;;                                  (song-title song)
  ;;                                  "300" "25")
  ;;    (cl-audioscrobbler:audioscrobbler-protocol-error (condition)
  ;;      (format t "Error ~A~%" condition)
  ;;      (cl-log:log-message :error
  ;;                          (format nil "[Last.fm] Protocol error ~A" condition)))
  ;;    (simple-error (condition)
  ;;      (format t "Error ~A~%" condition)
  ;;      )))
  )
