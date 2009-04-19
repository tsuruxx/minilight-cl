;;;;
;;;; MiniLight Common Lisp : minimal global illumination renderer
;;;; Copyright (c) 2007-2009, Harrison Ainsworth / HXA7241 and Charles McMackin.
;;;;
;;;; http://www.hxa7241.org

(in-package #:minilight)

(defparameter *max-triangles* (expt 2 20))
(defparameter *max-emitters* 16)

(defclass scene ()
  ((mesh             :accessor mesh
	             :initargs :mesh)
   
   (emitters         :accessor emitters
	             :initargs :emitters)
   
   (spacial-index    :accessor space-idx
		     :initargs :space-idx)
   
   (sky-emission     :acccessor sky-emission
		     :initargs :sky-emission
		     :type v3d:vector3d)
   
   (ground-reflection :accessor ground-reflect
		     :initargs :ground-reflect
		     :type v3d:vector3d)))

(defmethod make-initialize-instance :after
    ((scene scene) &keys mesh &allow-other-keys)
  (with-slots (emitters spatial-index) scene
    (setf emitters
	  (loop :for i :below (length mesh)
	     :while (< (length tmp) *max-emitters*)
	     :when (and (not (vector-zerop (emitivity tri)))
			(plusp (area tri)))
	     :collect (let ((i-val i))
			(lambda () (aref mesh i-val)))
	     :into tmp
	     :finally (return tmp)))))

(defun make-scene (in-stream eye-position)
  (let* ((sky-emission (read-vector in-stream))
	 (ground-reflection (read-vector in-stream))
	 (triangles (coerce (loop :for i :below *max-triangles* :collect
			       (make-triangle in-stream))
			    'vector)))
    (make-instance 'scene
		   :sky-emission sky-emission
		   :ground-reflect (vector* sky-emission ground-reflection)
		   :mesh triangles
		   :space-idx (make-spatial-index eye-position triangles))))

(defgeneric emitter (scene)) ;should probably be just (scene)
(defgeneric emitters-count (scene))
(defgeneric default-emission (scene back-direction))

(defmethod intersect-p ((ray obj) (scene scene))
  (intersect-p ray (spacial-idx scene)))

(defmethod emitter ((scene scene))
  (let ((len (emitters-count scene)))
    (when (plusp len)
      (let ((emitter (funcall (aref (emitters scene) (random len)))))
	(values (sample-point emitter) emitter)))))

(defmethod emitters-count ((scene scene))
  (length (emitters scene)))

(defmethod default-emission ((scene scene) back-direction)
  (if (< (aref back-direction 1) 0.0)
      (sky-emission scene)
      (ground-reflection scene)))