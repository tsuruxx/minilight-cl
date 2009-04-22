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

(defun is-branch-p (items level)
  (and (> (length items) *max-levels*)
       (< level (- *max-levels* 1))))



;; (defun find-bounds (array)
;;   (loop for vertex across array
;;      for x = (aref vertex 0)
;;      for y = (aref vertex 1)
;;      for z = (aref vertex 2)
;;      maximize x into max-x
;;      minimize x into min-x
;;      maximize y into max-y
;;      minimize y into min-y
;;      maximize z into max-z
;;      minimize z into min-z
;;      finally (return (values (list min-x min-y min-z)
;;                           (list max-x max-y max-z)))))

(defun bounds-p (test-vector bounding-vector)
  "tests whether all of TEST-VECTOR lies within BOUNDING-VECTOR
the min and max of each vector's space are represented by the 0-2 and 3-5
elements respectively."
  (and (>= (aref test-vector 3) (aref bounding-vector 0))
       (<  (aref test-vector 0) (aref bounding-vector 3))
       (>= (aref test-vector 4) (aref bounding-vector 1))
       (<  (aref test-vector 1) (aref bounding-vector 4))
       (>= (aref test-vector 5) (aref bounding-vector 2))
       (<  (aref test-vector 2) (aref bounding-vector 5))))

;; (defun bounded-by-p (vector1 vector2)
;;   (declare (type (vector single-float 6) vector1 vector2))
;;   "Tests whether VECTOR1 is bounded by VECTOR2"
;;   (and (every #'>= (subseq vector1 0 3) (subseq vector2 0 3))
;;        (every #'<= (subseq vector1 3) (subseq vector2 3))))

(defun bounded-by-p (aabb1 aabb2)
  (with-slots (lx ly lz hx hy hz) aabb1
    (with-slots ((lx2 lx) (ly2 ly) (lz2 lz)
                 (hx2 hx) (hy2 hy) (hz2 hz)) aabb2
      (and (every #'>= (list lx ly lz) (list lx2 ly2 lz2))
           (every #'< (list hx hy hz) (list hx2 hy2 hz2))))))

(defun find-bounded (array aabb)
  (coerce (loop for tri across array
             when tri
             when (bounded-by-p (bounds tri) aabb)
             collect tri)
          'vector))

(defun bounds-p2 (vector1 vector2)
  (and (every #'>= (subseq vector1 0 3) (subseq vector2 3))
       (every #'< (subseq vector1 3) (subseq vector2 0 3))))

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
                         :finally (return (make-aa-bbox (min eye-x a)
                                                        (min eye-y b)
                                                        (min eye-z c)
                                                        d e f)))))
      (make-spatial-index new-bound items))))

(defmethod make-spatial-index ((bounds aa-bbox) nodes)
  (let ((max-depth (- *max-levels* 1)))
    (labels ((low-tolerance-p (bbox)
               (< (- (hx bbox) (lx bbox))
                  (* +tolerance+ 4.0)))
             (make-node (bounds nodes depth)
               (if (and (< depth max-depth)
                        (> (length nodes) *max-items*)
                        (not (low-tolerance-p bounds)))
                   (make-instance 'spatial-index
                                  :bounds bounds
                                  :nodes  (coerce
                                           (loop for sub-bound in (subdivide bounds)
                                              collect
                                                (make-node sub-bound
                                                           (find-bounded nodes sub-bound)
                                                           (+ depth 1)))
                                           'vector))
                   (make-instance 'spatial-index :bounds bounds :nodes nodes))))
      (make-node bounds nodes 0))))

;;; ------------------- Intersection -------------------------------------------

;; (defmethod intersect-p ((ray slope-ray) (index spatial-index))
;;   ;; Intersection test between ray and octree
;;   ;; TODO: determine if a FIND-SUBCELL function is needed. For now, leave out.
;;   (with-slots (bounds nodes) index
;;     (with-slots (origin direction) ray
;;       (let ((distance (intersect-p ray bounds)))
;;      (when distance
;;        (let ((hit (hit-point ray distance)))
;;          ;; Maybe pull this out into the triangle intersect code.
;;          (when (and (in-bounds-p hit) )
;;            (intersect-p ray nodes))))))))


(defmethod intersect-p ((ray slope-ray) (index spatial-index))
  ;; Intersection test between ray and octree
  ;; TODO: determine if a FIND-SUBCELL function is needed. For now, leave out.
  (with-slots (bounds nodes) index
    (let ((distance (intersect-p ray bounds)))
      (when distance
        (intersect-p ray nodes)))))


(defmethod intersect-p ((ray slope-ray) (nodes vector))
  ;; Test each node until a hit.
  (some #'(lambda (node) (intersect-p ray node)) nodes))
