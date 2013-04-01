;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          file-selector.lisp
;;;; Purpose:       McClim file selector
;;;; Programmers:   Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :mcclim)
  (require :split-sequence)
  (require :cl-fad))


(cl:defpackage :clim-utils
  (:use :clim-lisp :clim :cl-fad)
  (:export #:select-file
           #:file-selector-pane))


(in-package :clim-utils)


(defun get-parent-directory (dir)
  "Get parent directory of DIR."
  (let ((l (pathname-directory dir)))
    (namestring (make-pathname :directory (remove (car (last l)) l)))))


(defun filter-directory (directory &optional filters)
  "List a directory applying FILTERS."
  (let ((data (cl-fad:list-directory directory))
        output)
    (if filters
        (loop for input in data
           if (cl-fad:directory-exists-p input)
           do (setf output (append output (list input)))
           else
           if (find (pathname-type input) filters :test #'string-equal)
           do (setf output (append output (list input))))
        (setf output data))
    output))


(define-application-frame file-selector ()
  ((parent :initform ""
           :initarg :parent
           :accessor file-selector-parent)
   (data :initform nil
         :initarg :data
         :accessor file-selector-data)
   (current :initform nil
            :initarg :current
            :accessor file-selector-current)
   (filters :initform '()
            :initarg :filters
            :accessor file-selector-filters))
  (:panes 
   (parent :application
           :display-function #'display-parent
           :max-height 20
           :scroll-bars nil
           :display-time :command-loop)
   (directory-pane :application
                   :display-function #'display-directory
                   :display-time :command-loop)
   (filename-pane :application
                   :display-function #'display-filename
                   :display-time :command-loop)
   (current-pane :application
                 :scroll-bars nil
                 :min-height 30
                 :incremental-redisplay t
                 :display-time :command-loop
                 :display-function #'display-selected)
;;    (filters-pane (make-pane 'option-pane
;;                             :items (file-selector-filters *application-frame*)
;;                             :value (file-selector-filters *application-frame*)))
   (ok-bt (make-pane 'push-button
                     :label "Ok"
                     :activate-callback #'ok-callback))
   (cancel-bt (make-pane 'push-button
                         :label "Cancel"
                         :activate-callback #'close-callback))
   )
  (:layouts
   (default
       (vertically (:equalize-width nil :equalize-height nil
                    :height 600 :width 650)
         (labelling (:label "Browse"
                            :text-style (make-text-style :sans-serif
                                                         :roman
                                                         :small)
                            :align-x :center
                            :align-y :center))
         +fill+
         (labelling (:label "Parent")
           parent)
         +fill+
         (labelling (:label "Browse")
           (horizontally ()
             directory-pane +fill+ filename-pane))
         +fill+
;;          (labelling (:label "Filter")
;;            filters-pane)
;;          +fill+
         (labelling (:label "Selected")
           current-pane)
         +fill+
         (labelling (:label "Control")
           (horizontally ()
             ok-bt +fill+ cancel-bt))
         ))))


(define-presentation-type dir-namestring ())


(define-presentation-method present (data (type dir-namestring) stream view &key)
  (declare (ignore type view))
  (updating-output (stream :unique-id data)
    (with-drawing-options
        (stream
         :text-style (make-text-style :sans-serif
                                      :bold
                                      :normal))
      (format stream "~A" data))))


(define-file-selector-command com-select-dir-namestring
    ((dir-namestring 'dir-namestring :gesture :select))
  (with-slots (parent current data filters) *application-frame*
    (let ((next (concatenate 'string (namestring current)
                             dir-namestring
                             "/")))
      (setf data (filter-directory next filters)
            parent current
            current next))))

(define-presentation-type filename-namestring ())


(define-presentation-method present (data (type filename-namestring) stream view &key)
  (declare (ignore type view))
  (format stream "~A" data))


(define-file-selector-command com-select-filename-namestring
    ((filename-namestring 'filename-namestring :gesture :select))
  (with-slots (current) *application-frame*
    (setf current
          (concatenate 'string (namestring current) filename-namestring))))


(define-presentation-type parent-namestring ())


(define-presentation-method present (data (type parent-namestring) stream view &key)
  (declare (ignore type view))
  (format stream "~A" data))


(define-file-selector-command com-select-parent-namestring
    ((parent-namestring 'parent-namestring :gesture :select))
  (with-slots (current parent data filters) *application-frame*
    (setf current parent-namestring
          parent (get-parent-directory parent-namestring)
          data (filter-directory parent-namestring filters))))


(defgeneric display-parent (frame stream)
  (:documentation "Display the parent directory."))


(defmethod display-parent ((frame file-selector) stream)
  (clear-output-record (stream-output-history stream))
  (present (file-selector-parent frame) 'parent-namestring :stream stream)
  (terpri stream))


(defgeneric display-directory (frame stream)
  (:documentation "Display input directory."))


(defmethod display-directory ((frame file-selector) stream)
  (clear-output-record (stream-output-history stream))
  (loop for elem in (file-selector-data frame)
     as tokens = (split-sequence:split-sequence #\/ (namestring elem)
                                                :remove-empty-subseqs t)
     when (cl-fad:directory-exists-p elem)
     do (present (car (last tokens)) 'dir-namestring :stream stream)
        (terpri stream)))


(defgeneric display-filename (frame stream)
  (:documentation "Display input filenames."))


(defmethod display-filename ((frame file-selector) stream)
  (clear-output-record (stream-output-history stream))
  (with-slots (data filters) frame
    (loop for elem in data
       as tokens = (split-sequence:split-sequence #\/ (namestring elem)
                                                  :remove-empty-subseqs t)
       unless (cl-fad:directory-exists-p elem)
       do (present (car (last tokens)) 'filename-namestring :stream stream)
          (terpri stream))))


(defgeneric display-selected (frame stream)
  (:documentation "Display information about current input data."))


(defmethod display-selected ((frame file-selector) stream)
  (clear-output-record (stream-output-history stream))
  (format stream "~A" (file-selector-current frame)))


(defun close-callback (button)
  "Callback to close dialog box."
  (finish-output *error-output*)
  (let ((frame (gadget-client button)))
    (setf (file-selector-current frame) nil)
    (frame-exit frame)))


(defun ok-callback (button)
  "Callback when a file is selected."
  (frame-exit (gadget-client button)))


(defun file-selector-pane (calling-frame
                           &key (current (user-homedir-pathname)) filters)
  "Return a file selector widget.
CURRENT is the first directory to browse.
FILTERS is a list of files' types."
  (let ((frame (make-application-frame 'file-selector
                                       :calling-frame calling-frame
                                       :filters filters
                                       :current current
                                       :data (filter-directory current filters)
                                       :parent (get-parent-directory current))))
    (run-frame-top-level frame)
    (file-selector-current frame)))


(defun select-file (&key (current (user-homedir-pathname)) filters)
  (let ((frame
         (make-application-frame 'file-selector
                                 :filters filters
                                 :current current
                                 :data (filter-directory current filters)
                                 :parent (get-parent-directory current))))
    (run-frame-top-level frame)
    (file-selector-current frame)))

