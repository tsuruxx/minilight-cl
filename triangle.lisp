;;;;
;;;; MiniLight Common Lisp : minimal global illumination renderer
;;;; Copyright (c) 2007-2009, Harrison Ainsworth / HXA7241 and Charles McMackin.
;;;;
;;;; http://www.hxa7241.org

(in-package #:minilight)

(defparameter +tolerance+ (/ 1.0 1024.0))

(defclass triangle ()
  ((vertices :accessor vertices
	     :initarg :vertices
	     :type v3d:vector3d) ;array of 3 verts
   
   (reflectivity :accessor reflectivity
		 :initarg :reflectivity
		 :type v3d:vector3d)
   
   (emitivity :accessor emitivity
	      :initarg :emitivity
	      :type v3d:vector3d)))

;; (defclass ray ()
;;   ((origin :accessor origin
;; 	   :initarg :origin
;; 	   :type v3d:vector3d)
   
;;    (direction :accessor direction
;; 	      :initarg :direction
;; 	      :type v3d:vector3d)))

;; (defun make-ray (origin direction)
;;   (make-instance 'ray :origin origin :direction direction))

(defun make-triangle (file-stream)
  (let* ((verts (vector (read-vector file-stream)
			(read-vector file-stream)
			(read-vector file-stream)))
	 (r (nvector-clamp (read-vector file-stream) (vec3-0) (vec3-max)))
	 (e (nvector-clamp (read-vector file-stream) (vec3-0) (vec3-max))))
    
    (when (and verts r e)
      (make-instance 'triangle :vertices verts :reflectivity r :emitivity e))))


;;(defgeneric bound (obj))
(defgeneric intersect-p (object1 object2))
(defgeneric sample-point (obj))
(defgeneric normal (obj))
(defgeneric tangent (obj))
(defgeneric area (obj))


(defmethod bounds ((tri triangle))
  (with-slots (vertices ) tri
   (let* ((v2 (aref vertices 2))
	 (bound (concatenate '(vector * 6) v2 v2)))
     (loop :for j below 3
	:for j+3 = (+ j 3)
	:for v0 = (aref (aref vertices 0) j)
	:for v1 = (aref (aref vertices 1) j)
	:do (symbol-macrolet ((bound-j (aref bound j))
			      (bound-j3 (aref bound j+3)))
	      (if (< v0 v1)
		  (progn (when (< v0 bound-j)
			   (setf bound-j v0))
			 (when (> v1 bound-j3)
			   (setf bound-j3 v1)))
		  (progn (when (< v1 bound-j)
			   (setf bound-j v1))
			 (when (> v0 bound-j3)
			   (setf bound-j3 v0))))
	      (decf bound-j (* (abs (+ bound-j 1.0)) +tolerance+))
	      (incf bound-j3 (* (abs (+ bound-j3 1.0)) +tolerance+))))
     (vec-aa-bbox bound))))




;; @implementation
;; Adapted from:
;; <cite>'Fast, Minimum Storage Ray-Triangle Intersection'
;; Moller, Trumbore;
;; Journal Of Graphics Tools, v2n1p21, 1997.
;; http://www.acm.org/jgt/papers/MollerTrumbore97/
;; or http://jgt.akpeters.com/papers/MollerTrumbore97/</cite>

(defmethod intersect-p ((ray ray) (tri triangle))
  (with-slots ((verts vertices)) tri
    (with-slots (ox oy oz dx dy dz) ray
      (let ((origin (vec3 ox oy oz))
	    (ray-dir (vec3 dx dy dz))
	    (v0 (aref verts 0))
	    (v1 (aref verts 1))
	    (v2 (aref verts 2))
	    (epsilon 1.0e-6))
	(let ((edge1 (vector- v1 v0))
	      (edge2 (vector- v2 v0)))
	  (let* ((pvec (cross ray-dir edge2))
		 (det (dot edge1 pvec)))
	    (if (and (> det (- epsilon)) (< det epsilon))
		nil
		(let* ((inverse-det (/ 1.0 det))
		       (tvec (vector- origin v0))
		       (u (* (dot tvec pvec) inverse-det)))
		  (if (or (< u 0.0) (> u 1.0))
		      nil
		      (let* ((qvec (cross tvec edge1))
			     (v (* (dot ray-dir qvec) inverse-det)))
			(if (or (< v 0.0) (> (+ u v) 1.0))
			    nil
			    (let ((hit-distance (* (dot edge2 qvec)
						   inverse-det)))
			      ;; values ?
			      (values (>= hit-distance 0.0) hit-distance)))))))))))))





(defmethod sample-point ((tri triangle))
  (let ((sqr1 (sqrt (random 1.0)))
	(r2 (random 1.0))
	(v0 (aref (vertices tri) 0))
	(v1 (aref (vertices tri) 1))
	(v2 (aref (vertices tri) 2)))
    (let ((a (- 1.0 sqr1))
	  (b (* (- 1.0 r2) sqr1))
	  (edge0 (vector- v1 v0))
	  (edge3 (vector- v2 v0)))
      
      (vector+ (vector* edge0 a) (vector* edge3 b) v0))))

(defmethod normal  ((tri triangle))
  (with-slots (verts vertices) tri
   (symbol-macrolet ((v1 (aref verts 1))
		     (v2 (aref verts 2)))
     
     (nnormalize (cross (tangent tri) (vector- v2 v1))))))

(defmethod tangent ((tri triangle))
  (with-slots (verts vertices) tri
   (symbol-macrolet ((v0 (aref verts 0))
		     (v1 (aref verts 1)))
     
     (nnormalize (vector- v0 v1)))))

(defmethod area ((tri triangle))
  (with-slots (verts vertices) tri
   (symbol-macrolet ((v0 (aref verts 0))
		     (v1 (aref verts 1))
		     (v2 (aref verts 2)))
     (let ((pa2 (cross (vector- v1 v0)
		       (vector- v2 v1))))
       
       (* 0.5 (sqrt (dot pa2 pa2)))))))
