;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          scan.lisp
;;;; Purpose:       Dialog for scaning directory to find music files.
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



(defun scan-close-callback (button)
  "Callback to close scan box."
  (clim:frame-exit (clim:gadget-client button)))


(defun browse-callback (button)
  "Callback which call a file browser."
  (let* ((frame (clim:gadget-client button))
         (dir (clim-utils:file-selector-pane frame
                                             :filters '("mp3" "ogg")))
         (text-field (clim:find-pane-named frame 'directory)))
    (when dir
      (setf (clim:gadget-value text-field)
            (princ-to-string (cl-ppcre:regex-replace-all "\\*\\.\\*"
                                                         (namestring dir)
                                                         ""))))))

(defun search-callback (button)
  "Search music files."
  (let* ((frame (clim:gadget-client button))
         (text-field (clim:find-pane-named frame 'directory))
         (stream (clim:frame-standard-output
                  (clim:pane-frame
                   (clim:find-pane-named frame 'output)))))
    (clim:updating-output (stream :unique-id 'output)
      (ernestine-tools:scan-directory
       (player-database (clim:frame-calling-frame frame))
       (clim:gadget-value text-field)
       :stream stream :type :io))))


(clim:define-application-frame scan ()
  ()
  (:panes
   (directory :text-field :value "" :activate-callback 'directory-callback)
   (output :application
           :incremental-redisplay t
           :display-function #'display-output)
   (browse-bt (make-button "Browse" 'browse-callback))
   (scan-bt (make-button "Search" 'search-callback))
   (ok-bt (make-button "Ok" 'scan-close-callback)))
  (:layouts
   (default
       (clim:vertically ()
         (clim:labelling (:label "Family" :align-x :center)
           (clim:vertically ()
             directory
             (clim:horizontally ()
               clim:+fill+
               browse-bt
               clim:+fill+
               scan-bt)))
         clim:+fill+
         output
         clim:+fill+
         (clim:horizontally (:equalize-width t)
           clim:+fill+ ok-bt clim:+fill+)))))



(defgeneric display-output (frame stream)
  (:documentation "Display output of search music files."))


(defmethod display-output ((frame scan) stream)
  )
