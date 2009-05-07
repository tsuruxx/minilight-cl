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
             :type vector) ;array of 3 verts
   
   (reflectivity :accessor reflectivity
                 :initarg :reflectivity
                 :type v3d:vector3d)
   
   (emitivity :accessor emitivity
              :initarg :emitivity
              :type v3d:vector3d)))

(defun make-triangle (file-stream)
  (let* ((verts (vector (read-vector file-stream)
                        (read-vector file-stream)
                        (read-vector file-stream)))
         (r (nvector-clamp (read-vector file-stream) (vec3-0) (vec3-max)))
         (e (nvector-clamp (read-vector file-stream) (vec3-0) (vec3-max))))
    
    (when (and verts r e)
      (make-instance 'triangle :vertices verts :reflectivity r :emitivity e))))


;;(defgeneric bound (obj))
#+nil
(defgeneric intersect-p (object1 object2 &optional last-hit)
  (:documentation
   "Tests intersection between object1 and object2
	RAY       -> SCENE
		Returns: intersect-p spatial-index
	SLOPE-RAY -> SPATIAL-INDEX
		Returns: (values out-direction color)
	SLOPE-RAY -> AA-BBOX
		Returns: hit-distance or nil
	SLOPE-RAY -> VECTOR
		Returns: list of sorted
	RAY       -> TRIANGLE
		Returns: (values (>= hit-distance 0.0) hit-distance
                                      tri (vec3 hit-distance u v))"))
;; (defgeneric sample-point (obj))
;; (defgeneric normal (obj))
;; (defgeneric tangent (obj))
;; (defgeneric area (obj))


(defun %bounds (vertices)
  (let* ((v2 (aref vertices 2))
         (bound (concatenate '(vector * 6) v2 v2)))
    (loop
       :for j below 3
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
    (vec-aa-bbox bound)))

(defun %bounds2 (vertices)
  (loop
     for vert across vertices
     maximizing (aref vert 0) into max-x
     minimizing (aref vert 0) into min-x
     maximizing (aref vert 1) into max-y
     minimizing (aref vert 1) into min-y
     maximizing (aref vert 2) into max-z
     minimizing (aref vert 2) into min-z
     finally (progn (format t "~a ~a ~a ~a ~a ~a" min-x min-y min-z max-x max-y max-z)
                    (return (make-aa-bbox min-x min-y min-z
                                          max-x max-y max-z)))))

(defmethod bounds ((tri triangle))
  (with-slots (vertices ) tri
    (%bounds vertices)))

;; @implementation
;; Adapted from:
;; <cite>'Fast, Minimum Storage Ray-Triangle Intersection'
;; Moller, Trumbore;
;; Journal Of Graphics Tools, v2n1p21, 1997.
;; http://www.acm.org/jgt/papers/MollerTrumbore97/
;; or http://jgt.akpeters.com/papers/MollerTrumbore97/</cite>

(defmethod intersect-p ((ray slope-ray) (tri triangle) &optional last-hit)
  (declare (ignore last-hit))
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
            (when (not (and (> det (- epsilon)) (< det epsilon)))
              (let* ((inverse-det (/ 1.0 det))
                     (tvec (vector- origin v0))
                     (u (* (dot tvec pvec) inverse-det)))
                  (when (not (or (< u 0.0) (> u 1.0)))
                    (let* ((qvec (cross tvec edge1))
                           (v (* (dot ray-dir qvec) inverse-det)))
                      (when (not (or (< v 0.0) (> (+ u v) 1.0)))
                        (let ((hit-distance (* (dot edge2 qvec)
                                               inverse-det)))
                          ;; values ?
                          (when (>= hit-distance 0.0)
                            (values hit-distance tri))))))))))))))







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
  (with-slots ((verts vertices)) tri
    (symbol-macrolet ((v1 (aref verts 1))
                      (v2 (aref verts 2)))
      
      (nnormalize (cross (tangent tri) (vector- v2 v1))))))

(defmethod tangent ((tri triangle))
  (with-slots ((verts vertices)) tri
    (symbol-macrolet ((v0 (aref verts 0))
                      (v1 (aref verts 1)))
      
      (nnormalize (vector- v0 v1)))))

(defmethod area ((tri triangle))
  (with-slots ((verts vertices)) tri
    (symbol-macrolet ((v0 (aref verts 0))
                      (v1 (aref verts 1))
                      (v2 (aref verts 2)))
      (let ((pa2 (cross (vector- v1 v0)
                        (vector- v2 v1))))
        
        (* 0.5 (sqrt (dot pa2 pa2)))))))
