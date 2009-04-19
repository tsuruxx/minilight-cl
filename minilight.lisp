(defpackage #:hxa7241_minilight
  (:nicknames #:minilight)
  (:use #:common-lisp #:vector-3d))

(in-package #:minilight)



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

;;file reading utils

(defun read-vector (stream)
  (let ((in (read stream)))
    (when in (coerce in 'vector))))


(defun main (args)
  (format t *banner-message*)
  (let* ((model-file-pathname (second args))
	 (image-file-pathname (concatenate 'string model-file-pathname ".ppm")))
    (let (format-id
	  iterations 
	  image 
	  camera
	  scene)
      ;; save-image helper fn
      (flet ((save-image (n frame-num)
	       (with-open-file (image-file model-file-pathname :direction :output)
		 (get-formatted image image-file (1- frame-num)))
	       (when (not n)
		 (format t "~%interrupted~&")
		 (return-from main))))
	;; open model file and read
	(with-open-file (model-file model-path)
	  (setf format-id (read-line model-file))
	  (if (not (string= *model-format-id* format-id))
	      (error "invalid model file")
	      (setf iterations  (read-line model-file)
		    image  (make-image model-file)
		    camera (make-camera model-file)
		    scene (make-scene model-file (eye-point camera)))))
	;; render loop
	(loop for frame-num from 1 to iterations
	   with last-time = (get-universal-time)
	   :do (frame camera scene image)
	   :do (let ((new-time (get-universal-time)))
		 (when (or (< *save-period* (- new-time last-time))
			   (= frame-num iterations))
		   (setf last-time new-time)
		   (save-image t frame-num)))
	   :do (format t "iteration: ~a" (frame-num))
	   :finally (format t "~%finished~&"))))))
