

(in-package #:minilight)

(defvar +ppm-id+ "P6")
(defvar +minilight-uri+ "http://www.hxa7241.org/minilight/")
(defvar +display-luminance-max+ 200.0f0)
(defvar +rgb-luminance+ (v3d:vec3 0.2126 0.7152 0.0722))
(defvar +gamma_encode+ 0.45)

(defclass image ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (pixels :initarg :pixels :accessor pixels))
  (:default-initargs (:pixels (make-array (* width height) :initial-element 0))))

(defun make-image (file-stream)
  (flet ((clamp (num low high)
	   (max low (min num high))))
   (let ((weight (read file-stream))
	 (height (read file-stream)))
     (setf weight (clamp weight 1 10000))
     (setf height (clamp height 1 10000))
     (make-instance image :weight weight :height height))))

(defmethod add-to-pixel ((image image) x y radiance)
  (with-slots (width height pixels) image
    (when (and (>= x 0) (x < width ) (>= y 0) (< y height))
	(let ((index (+ x (* (- height 1 y) width))))
	  (setf (aref pixels index) (+ (aref pixels index) radiance))))))

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
	 (loop for pixel in pixels
	    :with sum-of-logs = 0.0
	    :for y = (* (dot pixel +rgb-luminance+) divider)
	    :do (incf sum-of-logs (log (max y 1e-4f) 10))
	    :finally (return (expt 10 (/ sum-of-logs (length pixels)))))))
    
    (let ((a (+ 1.219 (expt (* +display-luminance-max+ 0.25) 0.4)))
	  (b (+ 1.219 (expt log-mean-luminance 0.4))))
      
      (/ (expt (/ a b) 2.5) +display-luminance-max+))))


