;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          music.lisp
;;;; Purpose:       MP3, OGG Vorbis management
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :ernestine-tools)



;; (defparameter *file-info* "/usr/bin/file"
;;   "Determine file type")


;; (defparameter *mp3-info* "/usr/bin/mp3info"
;;   "Command to extract id3tag informations from MP3 files")


;; (defparameter *mp3-info-args* "-p '%n;%t;%a;%l;%g;%y'")


;; (defparameter *ogg-info* "/usr/bin/ogginfo"
;;   "Command to extract informations from Ogg Vorbis files")


;; (defparameter *no-id3-v1* "does not have an ID3 1.x tag")



#+cmu
(defun run-shell-command (command args &key (output *standard-output*))
  (ext:process-exit-code
   (ext:run-program command args :input nil :output output)))


#+sbcl
(defun run-shell-command (command args &key (output *standard-output*))
  (sb-ext:process-exit-code
   (sb-ext:run-program command args :input nil :output output)))



(defmacro with-program-result ((result command args) &body body)
  "Macros which executes COMMAND with ARGS, and set the ouput in string RESULT."
  `(let ((stream (make-string-output-stream))
         (,result))
     (run-shell-command ,command ,args :output stream)
     (setf ,result (get-output-stream-string stream))
     ,@body))


(defstruct tag-infos
  title artist album date track genre)


(defun extract-mp3-tag (filename)
  "Extract tag informations from result of *mp3-info* command, and
creates a TAG-INFOS structure."
  (with-program-result (result *mp3-info* (list *mp3-info-args* filename))
    (unless (search *no-id3-v1* result :test #'string-equal)
      (let ((infos (split-sequence:split-sequence #\; 
                                                  (subseq result 2)
                                                  :remove-empty-subseqs t)))
        (make-tag-infos :title (second infos)
                        :artist (third infos)
                        :album (fourth infos)
                        :date (sixth infos)
                        :track (first infos)
                        :genre (fifth infos))))))


(defun read-property (string &optional (separator #\=))
  "Read STRING which is sequence of key=value. Creates a cons
like (key . value)."
  (let* ((tokens (string-trim '(#\Space #\Tab #\Newline) string))
         (index (position separator tokens)))
    (cons (subseq tokens 0 index)
          (subseq tokens (1+ index)))))


(defun property-key-value (property key)
  "Extract value of KEY in PROPERTY."
  (cdr (find key property :test #'string-equal :key #'car)))


(defun extract-ogg-tag (filename)
  "Executes the *ogg-info* command on FILENAME to extract OGG VORBIS
tags. Creates a TAG-INFOS structure."
   (with-program-result (result *ogg-info* (list filename))
     (let ((data
            (loop
               with strings = (split-sequence:split-sequence #\newline result
                                                             :remove-empty-subseqs t)
               for string in strings
               when (search "=" string)
               collect (read-property string))))
       (let* ((track-data (property-key-value data "TRACKNUMBER"))
              (rank (if (position #\/ track-data)
                        (car (read-property track-data #\/))
                        track-data)))
       (make-tag-infos :title (property-key-value data "TITLE")
                       :artist (property-key-value data "ARTIST")
                       :album (property-key-value data "ALBUM")
                       :date (property-key-value data "DATE")
                       :track (format nil "~A" (parse-integer rank))
                       :genre (property-key-value data "GENRE"))))))


(defun extract-info (filename &optional (stream *standard-output*))
  "Executes the file command on FILENAME to extract MP3 tags.
Return a list which contains song caracteristics :
 '(rank title artist album genre year)"
  ;;(format stream "Extract ~A~%" filename)
  (with-program-result (result *file-info* (list filename))
    (let ((song
           (cond ((search "MP3" result :test #'string-equal)
                  (extract-mp3-tag filename))
                 ((search "OGG" result :test #'string-equal)
                  (extract-ogg-tag filename))
                 )))
      song)))
