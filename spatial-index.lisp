;;;;
;;;; MiniLight Common Lisp : minimal global illumination renderer
;;;; Copyright (c) 2007-2009, Harrison Ainsworth / HXA7241 and Charles McMackin.
;;;;
;;;; http://www.hxa7241.org


 ;;; A minimal spatial index for ray tracing.<br/><br/>
 ;;;
 ;;; Suitable for a scale of 1 metre == 1 numerical unit, and has a resolution of
 ;;; 1 millimetre. (Implementation uses fixed tolerances)
 ;;;
 ;;; Constant.<br/><br/>
 ;;;
 ;;; @implementation
 ;;; A degenerate State pattern: typed by isBranch_m field to be either a branch
 ;;; or leaf cell.<br/><br/>
 ;;;
 ;;; Octree: axis-aligned, cubical. Subcells are numbered thusly:
 ;;; <pre>      110---111
 ;;;            /|    /|
 ;;;         010---011 |
 ;;;    y z   | 100-|-101
 ;;;    |/    |/    | /
 ;;;    .-x  000---001      </pre><br/><br/>
 ;;;
 ;;; Each cell stores its bound: fatter data, but simpler code.<br/><br/>
 ;;;
 ;;; Calculations for building and tracing are absolute rather than incremental --
 ;;; so quite numerically solid. Uses tolerances in: bounding triangles (in
 ;;; Triangle::getBound), and checking intersection is inside cell (both effective
 ;;; for axis-aligned items). Also, depth is constrained to an absolute subcell
 ;;; size (easy way to handle overlapping items).
 ;;;
 ;;; @invariants
 ;;; * bound_m[0-2] <= bound_m[3-5]
 ;;; * bound_m encompasses the cell's contents
 ;;; if isBranch_m
 ;;; * vector_m length is 8
 ;;; * vector_m elements are zero or SpatialIndex pointers
 ;;; else
 ;;; * vector_m elements are non-zero Triangle pointers

(in-package #:minilight)

(defparameter *max-levels* 44)
(defparameter *max-items* 8)

(defclass spatial-index ()
  ((bounds :initarg :bounds :accessor bounds)
   (nodes :initarg :nodes :accessor nodes)))

#+nil
(defun bounded-by-p (aabb1 aabb2)
  (with-slots (lx ly lz hx hy hz) aabb1
    (with-slots ((lx2 lx) (ly2 ly) (lz2 lz)
                 (hx2 hx) (hy2 hy) (hz2 hz)) aabb2
      (and (every #'>= (list lx ly lz) (list lx2 ly2 lz2))
           (every #'< (list hx hy hz) (list hx2 hy2 hz2))))))

(defun bounded-by-p (aabb1 aabb2)
  "Tests whether bounding box AABB1 lies within AABB2"
  (with-slots (lx ly lz hx hy hz) aabb1
    (with-slots ((lx2 lx) (ly2 ly) (lz2 lz)
                 (hx2 hx) (hy2 hy) (hz2 hz)) aabb2
      (and (every #'>= (list hx hy hz)  (list lx2 ly2 lz2))
           (every #'< (list lx ly lz) (list hx2 hy2 hz2))))))

#+nil
(defun find-bounded (array aabb)
  (loop
     :for tri :across array
     :if (bounded-by-p (bounds tri) aabb)
     :collect tri :into bounded
     :else :collect tri :into remainder
     :finally (return (cons (coerce bounded 'vector)
                           (coerce remainder 'vector))))
  
  ;;    (format t "f-b-b: ~s ~a~&" bounded (length bounded))
  ;;    (format t "f-b-r: ~s ~a~&" remainder (length remainder))
  )

(defun find-bounded (array aabb)
  (loop
     :for tri :across array
     :if (bounded-by-p (bounds tri) aabb)
     :collect tri :into bounded
     :finally (return (coerce bounded 'vector)))
  
  ;;    (format t "f-b-b: ~s ~a~&" bounded (length bounded))
  ;;    (format t "f-b-r: ~s ~a~&" remainder (length remainder))
  )

(defmethod make-spatial-index ((cam camera) items)
  (with-slots ((eye view-position)) cam
    (let* ((eye-x (aref eye 0))
           (eye-y (aref eye 1))
           (eye-z (aref eye 2))
           (new-bound (loop :for item :across items
                         :for bbox = (bounds item)
                         :minimize (lx bbox) :into a
                         :minimize (ly bbox) :into b
                         :minimize (lz bbox) :into c
                         :maximize (hx bbox) :into d
                         :maximize (hy bbox) :into e
                         :maximize (hz bbox) :into f
                         :finally (return
                                    (let* ((min-x (min eye-x a))
                                           (min-y (min eye-y b))
                                           (min-z (min eye-z c))
                                           (x-len (- d min-x))
                                           (y-len (- e min-y))
                                           (z-len (- f min-z))
                                           (size (max x-len y-len z-len)))
                                      (make-aa-bbox min-x min-y min-z
                                                    (max d (+ min-x size))
                                                    (max e (+ min-y size))
                                                    (max f (+ min-z size))))))))
      (make-spatial-index new-bound items))))



;;3b's:
#+nil
(defmethod make-spatial-index ((bounds aa-bbox) nodes)
  (let ((max-depth (- *max-levels* 1)))
    (labels ((low-tolerance-p (bbox)
               (< (- (hx bbox) (lx bbox))
                  (* +tolerance+ 4.0)))
             (make-node (bounds nodes depth)
               (if (and (< depth max-depth)
                        (> (length nodes) *max-items*)
                        (not (low-tolerance-p bounds)))
                   (let ((sub-nodes (coerce
                                     (loop for sub-bound in (subdivide bounds)
                                        for (b o) = (loop for tri across nodes
                                                       if (bounded-by-p (bounds tri) sub-bound)
                                                       collect tri into b
                                                       else collect tri into o
                                                       finally
                                                         (return
                                                           (list
                                                            (coerce b 'vector)
                                                            (coerce o 'vector))))
                                        do (setf nodes o)
                                        collect
                                          (make-node sub-bound
                                                     b (+ depth 1))
                                        into children
                                        finally (return (append children (coerce nodes 'list))))
                                     'vector)))
                     (make-instance 'spatial-index :bounds bounds :nodes sub-nodes))
                   (make-instance 'spatial-index :bounds bounds :nodes nodes))))
      (make-node bounds nodes 0))))


(defmethod make-spatial-index ((bounds aa-bbox) nodes)
  (let ((degenerate-limit 0)
        (max-depth (- *max-levels* 1)))
    (labels
        ((degenerate-p (sub-items)
           (when (= (length nodes) (length sub-items))
             (incf degenerate-limit))
           (> degenerate-limit 1))
         
         (low-tolerance-p (bbox)
           (< (- (hx bbox) (lx bbox))
              (* +tolerance+ 4.0)))
         
         (make-sub-node-p (bounds nodes depth)
           (and (< depth max-depth)
                (> (length nodes) 8)
                (not (low-tolerance-p bounds))))
         
         (make-node (bounds nodes depth)
           (cons bounds
                 (if (make-sub-node-p bounds nodes depth)
                     (loop
                        :with sub-nodes = (make-array 8)
                        :for sub-node :below 8
                        :for sub-bound :across (coerce (subdivide bounds) 'vector)
                        :for bounded = (find-bounded nodes sub-bound)
                        :when (not (degenerate-p bounded))
                        :do (setf (aref sub-nodes sub-node)
                                  (if (zerop (length bounded))
                                      nil
                                      (make-node sub-bound bounded (+ depth 1))))
                        :finally (return sub-nodes))
                     nodes))))
      (let ((octree (make-node bounds nodes 0)))
        ;;  (format t "~s~%" octree)
        #+nil
        (make-instance 'spatial-index
                       :bounds (car octree)
                       :nodes (cdr nodes))
        
        octree))))

;;; ------------------- Intersection -------------------------------------------


#+nil
(defmethod intersect-p ((ray slope-ray) (index spatial-index))
  ;; Intersection test between ray and octree
  ;; TODO: determine if a FIND-SUBCELL function is needed. For now, leave out.
  (with-slots (bounds nodes) index
    (let ((distance (intersect-p ray bounds)))
      (when (and distance (not (zerop distance)) (plusp (decf *foo*)))
        (format t "i: ~s~%" distance))
      (when distance
        (intersect-p ray nodes)))))

(defparameter *foo* 10)
(defparameter *foo2* 10)


(defmethod intersect-p ((ray slope-ray) (nodes vector) &optional (last-hit nil))
  (with-slots (ox oy oz dx dy dz) ray
   ;; Test each node until a hit.
    (let ((hits (loop
                   for node across nodes
                   unless (eql node last-hit)
                   when (let ((r (multiple-value-list (intersect-p ray node))))
                          (when (first r) r))
                   collect it)))
      (when (and hits (plusp (decf *foo2*)))
        (format t "i: ~s~%" hits))
      
      (when hits
        ;;if it's coming from a triangle node it's hit-dist
        ;; but if it's coming from a cons node
        (let ((nearest-hit 
               (first (sort hits #'< :key (lambda (a) (abs (first a)))))))
          (if (= (length nearest-hit) 2)
            (let* ((origin (vec3 ox oy oz))
                   (direction (vec3 dx dy dz))
                   (hit-pos (vector+ origin (vector* direction (first nearest-hit)))))
              (values-list (append nearest-hit (list hit-pos))))
            (values-list nearest-hit))
          )))))

(defmethod intersect-p ((ray slope-ray) (index cons) &optional (last-hit nil))
  (with-slots (ox oy oz dx dy dz) ray
    (let ((distance (intersect-p ray (car index))))
      (when distance
        (multiple-value-bind (hit-dist tri-ref hit-pos)
            (intersect-p ray (cdr index) last-hit)
          (when hit-dist
;;;FIXME: check bounds tolerance on hit-pos?
            (values hit-dist tri-ref hit-pos))
          )))))

(defmethod intersect-p ((ray slope-ray) (obj2 t) &optional (last-hit nil))
  nil)
 