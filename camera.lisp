;;;; MiniLight Common Lisp : minimal global illumination renderer
;;;;
;;;; Copyright (c) 2007-2009, Harrison Ainsworth / HXA7241 and Charles McMackin.
;;;; http://www.hxa7241.org


;;camera makes the raytracer
(in-package #:minilight)

(defclass camera ()
  ((view-position :initarg view-position :reader eye-point)
   (view-angle :initarg :view-angle)
   (view-direction :initarg :view-direction)
   (right :initarg :right)
   (up :initarg :up)))

(defun make-camera (file-stream)
  (flet ((1/-1 (n)
	   (if (minusp n) 1.0 -1.0)))
    (let* ((vpos (read-vector file-stream))
	   (vdir (unitize (read-vector file-stream)))
	   (vang (read-vector file-stream))
	   (up (vec3 0.0 1.0 0.0)))
      (when (vec-zerop vdir)
	(setf vdir (vec3-0)))
      (setf vang (* (min 160.0 (max 10.0 vang))
		    (/ pi 180.0)))
      (let ((right (unitize (cross up vdir))))
	(if (vec-zerop right)
	    (setf up (unitize (cross vdir right)))
	    (progn
	      (setf up (vec3 0.0 0.0 (1/-1 (aref vdir 1))))
	      (setf right (unitize (cross up vdir)))))
	(make-instance 'camera
		       :view-position vpos :view-angle vang
		       :view-direction vdir :right right
		       :up up)))))



(defmethod frame ((camera camera) (scene scene) (image image))
  (with-slots ((view-dir view-direction)
	       (view-ang view-angle)
	       (view-pos view-position)) camera
    (with-slots (width height) image
      ;; make raytracer
      (let ((raytracer (make-raytracer scene))
	    (aspect (/ height width)))
	(loop :for y :from height :downto 0.0
	   :do (loop :for x :from width :downto 0.0
		  :for xf = (- (* (+ x (random 1.0))
				  (/ 2.0 width))
			       1.0)
		  
		  :for yf = (- (* (+ y (random 1.0))
				  (/ 2.0 height))
			       1.0)
		  
		  :for offset = (vector+ (vector-s* right xf)
					 (vector-s* up (*yf aspect)))
		  
		  :for sample-direction = (unitize
					   (vector+ view-dir
						    (vector-s* offset
							       (tan (* view-ang 0.5)))))
		  :for ray = (make-slope-ray view-pos sample-direction)
		  
		  :for radiance = (radiance raytracer ray)

		  :do (add-to-pixel image x y radiance)))))))