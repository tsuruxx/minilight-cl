(defpackage #:hxa7241_minilight
  (:nicknames #:minilight)
  (:use #:common-lisp #:vector-3d))

(in-package #:minilight)

(declaim (optimize (debug 3) (speed 2)))

(defvar *banner-message* "~%  MiniLight 1.5.2 Common Lisp
  Copyright (c) 2006-2008, Harrison Ainsworth / HXA7241 and Charles McMackin.
  http://www.hxa7241.org/minilight/ ~&")

(defvar *help-message*
  "~%----------------------------------------------------------------------
  MiniLight 1.5.2 Common Lisp


  Copyright (c) 2006-2008, Harrison Ainsworth / HXA7241 and Charles McMackin
  http://www.hxa7241.org/minilight/

  2009-3-29
----------------------------------------------------------------------

MiniLight is a minimal global illumination renderer.

usage:
  minilight modelFilePathName

The model text file format is:
  #MiniLight

  iterations

  imagewidth imageheight

  viewposition viewdirection viewangle

  skyemission groundreflection
  vertex0 vertex1 vertex2 reflectivity emitivity
  vertex0 vertex1 vertex2 reflectivity emitivity
  ...

-- where iterations and image values are ints, viewangle is a float,
and all other values are three parenthised floats. The file must end
with a newline. Eg.:
  #MiniLight

  100

  200 150

  (0 0.75 -2) (0 0 1) 45

  (3626 5572 5802) (0.1 0.09 0.07)
  (0 0 0) (0 1 0) (1 1 0)  (0.7 0.7 0.7) (0 0 0)~&" )

(defvar *model-format-id* "#MiniLight")
(defvar *save-period* 180)
(defparameter +pi+ (float pi 1f0))
;;file reading utils

(defun read-vector (stream)
  (let ((in (read stream nil 'eof)))
    (when (not (eql in 'eof)) (coerce (mapcar 'float in) 'vector3d))))

(defun valid-minilight-file-p (stream)
  (let ((line (string-trim '(#\Space #\Return #\Tab #\Newline)
                           (read-line stream))))
    (string= line *model-format-id*)))




;; (defun read-minilight-file (path-name)
;;   (with-open-file (in path-name)
;;     (when (valid-minilight-file-p in)
;;       (flet (read-triangles )))))

(defun main (file-name)
  (format t *banner-message*)
  (let* ((model-file-pathname file-name)
         (image-file-pathname (concatenate 'string model-file-pathname ".lisp.ppm")))
    ;; save-image helper fn
    (flet ((save-image (stream image n frame-num)
             (write-image image stream (1- frame-num))
             (when (not n)
               (format t "~%interrupted~&")
               (return-from main))))
      ;; open model file and read
      (with-open-file (in-model model-file-pathname)
        (with-open-file (out-image image-file-pathname
                                        ;   :element-type '(unsigned-byte 8)
                                   :direction :output
                                   :if-exists :supersede)
          (if (not (valid-minilight-file-p in-model))
              (error "invalid model file")
              (let* ((iterations  (read in-model))
                     (image       (make-image in-model))
                     (camera      (make-camera in-model))
                     (scene       (make-scene in-model camera)))
                ;; render loop
                (loop for frame-num from 1 to iterations
                   with last-time = (get-universal-time)
                   :do (frame camera scene image)
                   :do (let ((new-time (get-universal-time)))
                         (when (or (< *save-period* (- new-time last-time))
                                   (= frame-num iterations))
                           (setf last-time new-time)
                           (save-image out-image image t frame-num)))
                   :do (format t "~%iteration: ~a~&" frame-num)
                   :finally (format t "~%finished~&")))))))))
