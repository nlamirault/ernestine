;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          about.lisp
;;;; Purpose:       About dialog box
;;;; Programmer:    nicolas lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :ernestine-gui)



;; (defun about-callback (button)
;;   "Callback to display about box."
;;   (let ((frame (clim:gadget-client button)))
;;     (clim:run-frame-top-level
;;      (clim:make-application-frame 'about
;;                                   :height 230
;;                                   :width 450
;;                                   :calling-frame frame
;;                                   :doc-title "Ernestine About box"))))


(defmethod display-about-text (frame stream)
  (declare (ignore frame))
  (let* ((system (asdf:find-system "ernestine"))
         (author "Author : Nicolas Lamirault")
         (copyright "Copyright (C) Nicolas Lamirault")
         (version (if system
                      (asdf:component-version system)
                      ""))
        (description "Ernestine is a music organizer."))
    (clim:with-text-family (stream :serif)
      (format stream "~&~A~&~A~&Version : ~A~%~%~&~A"
              author copyright version description))))


(clim:define-application-frame about ()
  ()
  (:panes
   (about-pane :application
               :display-function #'display-about-text
               :incremental-redisplay t
               :initial-cursor-visibility nil)
   (ok-bt (make-button "Ok" 'close-box-callback)))
  (:layouts
   (default
       (clim:vertically (:height 10 :width 350)
         about-pane ok-bt))))
