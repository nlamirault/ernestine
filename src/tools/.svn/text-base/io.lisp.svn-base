;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          io.lisp
;;;; Purpose:       Scan directories, and update music database
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;; Date Started:  
;;;;
;;;; This file, part of ernestine, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ernestine users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(in-package :ernestine-tools)


(defparameter *music-types* '("mp3" "ogg")
  "Available format of music")


(defparameter *image-types* '("jpeg" "jpg" "png")
      "Available format of images")


(defun list-directory (dir)
  "List content of DIR"
  (let ((wildcard (make-pathname :name :wild
                                 :type :wild
                                 :defaults dir)))
    (directory wildcard)))


(defun get-file-extension (file)
  "Returns the extension of FILE"
  (car (last (split-sequence:split-sequence #\. file))))


(defun directory-p (token)
  "Returns TRUE if TOKEN is a directory."
  (string-equal (directory-namestring token) token))


(defun extract-data (directory)
  "Extract directories name from directory"
  (loop for dir in (list-directory directory)
     when (directory-p (namestring dir))
     collect (car (last (pathname-directory dir)))))


(defun extract-files (directory)
  "Extract entries from a DIRECTORY"
  (let (musics images)
    (loop for file in (list-directory directory)
       as file-name = (file-namestring file)
       as ext = (get-file-extension file-name)
       when (find ext *music-types* :test #'string-equal)
       do (push file-name musics)
       when (find ext *image-types* :test #'string-equal)
       do (push file-name images))
    (values (reverse musics) (reverse images))))


(defun scan-directory (backend directory
                       &key (stream *standard-output*) (type :taglib) covers-p)
  "Create music database. DIRECTORY contains music file. We use *mp3-info* and
*ogg-info* to extract informations of music files."
  (loop for dir in (list-directory directory)
     as name = (namestring dir)
     do
     (if (directory-p name)
         (scan-directory backend dir :stream stream :type type :covers-p covers-p)
         (progn
           (format stream "Analyse ~A ~%" name)
           (let ((song-tags (if (equal :taglib type)
                                ;;(extract-tag name nil)
                                (extract-info name stream)
                                (extract-info name stream))))
             (when song-tags
               (handler-case
                   (ernestine-database:make-artist backend
                                                   (tag-infos-artist song-tags)
                                                   nil)
                 (existing-artist-error (condition)
                   (format stream "~A~%" condition)))
               (handler-case
                   (let ((old (get-artist-album backend
                                                (tag-infos-artist song-tags)
                                                (tag-infos-album song-tags))))
                     (unless old
                       (let ((covers
                              (if covers-p
                                  (cl-covers:search-album-covers (tag-infos-artist song-tags)
                                                                 (tag-infos-album song-tags))
                                  nil)))
                         (ernestine-database:make-album backend
                                                        (tag-infos-artist song-tags)
                                                        (tag-infos-album song-tags)
                                                        (tag-infos-genre song-tags)
                                                        (tag-infos-date song-tags)
                                                        (first covers)
                                                        nil))))
                 (existing-album-error (condition)
                   (format stream "~A~%" condition)))
               (handler-case
                   (ernestine-database:make-song backend
                                                 (tag-infos-artist song-tags)
                                                 (tag-infos-album song-tags)
                                                 (tag-infos-title song-tags)
                                                 (tag-infos-track song-tags)
                                                 nil
                                                 name)
                 (existing-song-error (condition)
                   (format stream "~A~%" condition)))))))))


             
