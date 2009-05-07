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
                     :initarg :mesh)
   
   (emitters         :accessor emitters
                     :initarg :emitters)
   
   (spatial-index    :accessor space-idx
                     :initarg :space-idx)
   
   (sky-emission     :accessor sky-emission
                     :initarg :sky-emission
                     :type v3d:vector3d)
   
   (ground-reflection :accessor ground-reflect
                      :initarg :ground-reflect
                      :type v3d:vector3d)))

(defun %find-emitters (mesh)
  (loop
     :for tri :across mesh
     :while (< (length tmp) *max-emitters*)
     :when (and (not (vector-zerop (emitivity tri)))
                (plusp (area tri)))
     :collect tri
     :into tmp
     :finally (return (coerce tmp 'vector))))

#+nil
(defmethod make-initialize-instance :after
    ((scene scene) &key mesh &allow-other-keys)
  (with-slots (emitters spatial-index) scene
    (setf emitters (%find-emitters mesh))
    (format t "emitters: ~s~%" emitters)))

(defun make-scene (in-stream eye-position)
  (let* ((sky-emission (read-vector in-stream))
         (ground-reflection (read-vector in-stream))
         (triangles (coerce (loop :for i :below *max-triangles* 
                               :for tri =
                               (make-triangle in-stream)
                               :until (null tri) :collect tri)
                            'vector)))
    (make-instance 'scene
                   :emitters (%find-emitters triangles)
                   :sky-emission (nvector-clamp sky-emission (vec3-0) sky-emission)
                   :ground-reflect (vector* sky-emission
                                            (nvector-clamp ground-reflection
                                                           (vec3-0)
                                                           (vec3 1.0 1.0 1.0)))
                   :mesh triangles
                   :space-idx (make-spatial-index eye-position triangles))))

;; (defgeneric emitter (scene)) ;should probably be just (scene)
;; (defgeneric emitters-count (scene))
;; (defgeneric default-emission (scene back-direction))

(defmethod intersect-p ((ray slope-ray) (scene scene) &optional (last-hit nil))
  (with-slots ((index spatial-index)) scene
    (intersect-p ray index last-hit)))

(defmethod emitter ((scene scene))
  (with-slots (emitters) scene
    (let ((len (emitters-count scene)))
      (when (plusp len)
        (let ((emitter (aref emitters (random len))))
          (values (sample-point emitter) emitter))))))

(defmethod emitters-count ((scene scene))
  (length (emitters scene)))

(defmethod default-emission ((scene scene) back-direction)
  (with-slots (sky-emission ground-reflection) scene
    (if (minusp (aref back-direction 1))
        sky-emission 
        ground-reflection)))