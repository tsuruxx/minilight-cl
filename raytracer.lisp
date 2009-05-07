;;;; MiniLight Common Lisp : minimal global illumination renderer
;;;;
;;;; Copyright (c) 2007-2009, Harrison Ainsworth / HXA7241 and Charles McMackin.
;;;; http://www.hxa7241.org

(in-package #:minilight)

(defclass raytracer ()
  ((scene :initarg :scene :reader scene)))

(defun make-raytracer (scene)
  (make-instance 'raytracer :scene scene))

;; (defgeneric radiance (ray))
;; (defgeneric sample-emitters (ray))

(defmethod radiance ((raytracer raytracer) (ray slope-ray) &optional (last-hit nil))
  (with-slots (scene) raytracer
    (with-slots (ox oy oz dx dy dz) ray
      (multiple-value-bind (hit-dist hit-ref hit-position)
          (intersect-p ray scene last-hit)
        (declare (ignore hit-dist))
        (let ((-direction (vec3 (- dx) (- dy) (- dz))))
;;          (format t "ray=~s  hit-p=~a~%" (classification ray) hit-p)
          (if hit-position
              (let* ((surface-point (make-surface-point hit-ref hit-position))
                     (origin (vec3 ox oy oz))
                     (local-emission (if last-hit
                                         (vec3-0)
                                         (emission surface-point
                                                   origin -direction nil)))
                     (illumination
                      (sample-emitters raytracer ray surface-point)))
                (multiple-value-bind (next-direction color)
                    (next-direction surface-point -direction)
                  (let ((reflection
                         (if (vector-zerop next-direction)
                             next-direction
                             (vector* color
                                      (radiance raytracer
                                                (apply #'make-slope-ray
                                                       (concatenate 'list
                                                                    (^position surface-point)
                                                                    next-direction))
                                                (triangle-ref surface-point))))))
                    (vector+ reflection illumination local-emission))))
              (default-emission scene -direction)))))))

(defmethod sample-emitters ((raytracer raytracer) (ray slope-ray) surface-point)
  (with-slots (scene) raytracer
    (with-slots (dx dy dz) ray
      (with-slots ((surf-position position) triangle-ref) surface-point
        (multiple-value-bind (emitter-position emitter-ref)
            (emitter scene)
          (let ((emitter-reflection (vec3-0)))
            (when emitter-ref
             (let ((emit-direction
                    (nnormalize (vector- emitter-position surf-position))))
               (multiple-value-bind (hit-dist hit-ref hit-pos)
                   (let ((new-ray (apply #'make-slope-ray
                                         (concatenate 'list
                                                      surf-position
                                                      emit-direction))))
                     (intersect-p new-ray triangle-ref))
                 (declare (ignore hit-pos))
                 (when (or (not hit-dist) (equalp emitter-ref hit-ref))
                   (let* ((new-spoint (make-surface-point emitter-ref
                                                          emitter-position))
                          (emission-in (emission new-spoint surf-position
                                                 (vector- emit-direction)  t))
                          (-direction (vec3 (- dx) (- dy) (- dz))))
                     (setf emitter-reflection
                           (reflection surface-point
                                       emit-direction
                                       (vector* emission-in (float (emitters-count scene)))
                                       -direction)))))))
            emitter-reflection))))))

