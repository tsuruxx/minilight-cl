

(in-package #:minilight)

(defvar +ppm-id+ "P6")
(defvar +minilight-uri+ "http://www.hxa7241.org/minilight/")
(defvar +display-luminance-max+ 200.0f0)
(defvar +rgb-luminance+ (v3d:vec3 0.2126 0.7152 0.0722))
(defvar +gamma-encode+ 0.45)

(defclass image ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (pixels :initarg :pixels :accessor pixels))
  (:default-initargs :width 1 :height 1))

;; dunno why this isn't working...with-slots not available at this time?
;; (defmethod make-initialize-instance :after
;;     ((img image) &key width height &allow-other-keys)
;;   (with-slots (pixels) img
;;     (let ((size (* width height)))
;;       (setf pixels (make-array size
;; 			       :initial-contents (make-list size :initial-element (vec3-0)))))))

(defun make-image (file-stream)
  (let* ((w (clamp (read file-stream) 1 10000))
	 (h (clamp (read file-stream) 1 10000))
	 (size (* w h))
	 (pixels (coerce (make-list size :initial-element (vec3-0))
			 'vector)))
;;    (format t "~s" pixels)
    (make-instance 'image :width w :height h :pixels pixels)))

(defmethod add-to-pixel ((image image) x y radiance)
  (with-slots (width height pixels) image
    (when (and (>= x 0) (< x width ) (>= y 0) (< y height))
      (let ((index (+ x (* (- height 1 y) width))))
	(setf (aref pixels index) (vector+ (aref pixels index) radiance))))))

(defmethod write-image ((image image) file-out iteration)
  (with-slots (width height pixels) image
    (let* ((divider (+ 1 (/ 1.0 (max 0 iteration))))
	   (tone-map-scaling (calculate-tone-mapping pixels divider)))
      (format file-out "~a~&# ~a~&" +ppm-id+ +minilight-uri+)
      (format file-out "~%~a ~a~&255~&" width height)
      (loop for pixel across pixels
	 :do (loop for hue below 3
		:for mapped = (* (aref pixel hue) divider tone-map-scaling)
		:do (progn
		      (setf mapped (expt (max mapped 0.0) +gamma-encode+))
		      (setf mapped (floor (+ (* mapped 255.0) 0.5) ))
		      (write (min mapped 255.0) :stream file-out)))))))

(defun calculate-tone-mapping (pixels divider)
  (let ((log-mean-luminance 
	 (loop for pixel :across pixels
	    :with sum-of-logs = 0.0
	    :for y = (* (dot pixel +rgb-luminance+) divider)
	    :do (incf sum-of-logs (log (max y 0.0001) 10))
	    :finally (return (expt 10 (/ sum-of-logs (length pixels)))))))
    
    (let ((a (+ 1.219 (expt (* +display-luminance-max+ 0.25) 0.4)))
	  (b (+ 1.219 (expt log-mean-luminance 0.4))))
      
      (/ (expt (/ a b) 2.5) +display-luminance-max+))))


