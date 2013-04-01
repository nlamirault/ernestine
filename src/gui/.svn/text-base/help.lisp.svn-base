;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          help.lisp
;;;; Purpose:       Help dialog box
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


(defparameter *commands-description*
  '(("c-q" . "Exit Ernestine")
    ("c-h" . "Display the help message")
    ("c-Return" . "Start playing the selected song")
    ("c-Space" . "Stop playing the selected song")
    ("c-p" . "Play the previous song")
    ("c-n" . "Play the next song")
    ("c-a" . "Add the selected song to the playlist")
    ("c-d" . "Delete the selected song from the playlist")
    ("c-z" . "Randomize the playlist")
    ("c-i" . "Open a window dialog to search for music content"))
  "A description of available user's commands.")


;; Feature : clim:tabling 
(defmethod display-help-text (frame stream)
  (declare (ignore frame))
    (clim:with-text-family (stream :serif)
      (clim:with-drawing-options
          (stream :text-style (clim:make-text-style
                               :sans-serif
                               :bold
                               :normal))
        (format stream "~&Ernestine commands :~%~%"))
      (loop for help in *commands-description*
         do (format stream "~&~15A : ~A" (car help) (cdr help)))))
          

(clim:define-application-frame help ()
  ()
  (:panes
   (help-pane :application
              :display-function #'display-help-text
              :incremental-redisplay t
              :initial-cursor-visibility nil)
   (ok-bt (make-button "Ok" 'close-box-callback)))
  (:layouts
   (default
       (clim:vertically (:height 10 :width 350)
         help-pane ok-bt))))


;; (defun help-callback (button)
;;   "Callback to display about box."
;;   (let ((frame (clim:gadget-client button)))
;;     (clim:run-frame-top-level
;;      (clim:make-application-frame 'help
;;                                   :height 330
;;                                   :width 400
;;                                   :calling-frame frame
;;                                   :doc-title "Ernestine - Help"))))
