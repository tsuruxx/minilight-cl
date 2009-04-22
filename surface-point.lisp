;;;; MiniLight Common Lisp : minimal global illumination renderer
;;;;
;;;; Copyright (c) 2007-2009, Harrison Ainsworth / HXA7241 and CM.
;;;; http://www.hxa7241.org

(in-package #:minilight)

(defclass surface-point () 
  ((triangle-ref :initarg :triangle-ref :reader triangle-ref)
   (position :initarg :position :reader ^position)))

(defun make-surface-point (triangle position)
  (make-instance 'surface-point :triangle-ref triangle :position position))

(defmethod emission ((surface-point surface-point) to-position out-direction solid-angle-p)
  (with-slots ((tri triangle-ref) (pos position)) surface-point
    (let* ((distance2 (distance^2 to-position pos))
           (cos-area (* (dot out-direction (normal tri))
                        (area tri)))
           (solid-angle (if solid-angle-p
                            (/ cos-area (max distance2 1e-6))
                            1.0)))
      (if (plusp cos-area)
          (vector* (emitivity tri) solid-angle)
          (vec3-0)))))

(defmethod reflection ((surface-point surface-point) in-direction in-radiance out-direction)
  (with-slots ((tri triangle-ref)) surface-point
    (macrolet ((truth-xor (a b)
                 (let ((aval (gensym))
                       (bval (gensym)))
                   `(let ((,aval ,a)
                          (,bval ,b))
                      (and (or ,bval ,aval)
                           (not (and ,bval ,aval)))))))
      (let ((in-dot (dot in-direction (normal tri)))
            (out-dot (dot out-direction (normal tri))))
        (if (truth-xor (minusp in-dot) (minusp out-dot))
            (vec3-0)
            (vector* (vector* in-radiance (reflectivity tri))
                     (/ (abs in-dot) pi)))))))


(defmethod next-direction ((surface-point surface-point) in-direction)
  (with-slots ((tri triangle-ref)) surface-point
    (let ((reflectivity-mean (* (dot (refectivity tri) (one-vector3f)) (/ 1.0 3.0))))
      (if (< (random 1.0) reflectivity-mean)
          (let* ((color (vector* (reflectivity tri) (/ 1.0 reflectivity-mean)))
                 (2pr1 (* pi 2.0 (random 1.0)))
                 (sr2 (sqrt (random 1.0)))
                 (x (* (cos 2pr1) sr2))
                 (y (* (sin 2pr1) sr2))
                 (z (sqrt (- 1.0 (expt sr2 2))))
                 (normal (normal tri))
                 (tangent (tangent tri)))
            (when (< (dot normal in-direction) 0.0)
              (ninverse normal))
            (let ((out-direction (vector+ (vector* tangent x)
                                          (vector* (cross normal tangent) y)
                                          (vector* normal z))))
              (values out-direction color)))
          (values (vec3-0) (vec3-0))))))
