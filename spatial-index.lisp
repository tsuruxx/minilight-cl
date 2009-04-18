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
  (and (> (length items) +max-levels+)
       (< level (- +max-levels+ 1))))

(defmethod make-spatial-index ((cam camera) items level)
  (with-slots ((eye view-position)) cam
    (let ((new-bound (loop :for item :across items
			:for x = (bounds item)
			:minimize (aref x 0) :into a
			:minimize (aref x 1) :into b
			:minimize (aref x 2) :into c
			:maximize (aref x 3) :into d
			:maximize (aref x 4) :into e
			:maximize (aref x 5) :into f
			:finally (return (make-aa-box a b c d e f)))))
      (make-spatial-index new-bound items))))

(defmethod make-spatial-index ((octree spatial-index) items level)
  (with-slots (bounds nodes) octree
    (if (is-branch-p items level)
	(let ((q1 0))
	  (loop for ())))
    ))
(defmethod make-spatial-index ((tri triangle) items level)
  (with-slots (bound ) tri))

(defun make-spatial-index (vector items)
  (typecase vector
    (vector3d (let ((items (....))
		    (bounds (...))
		    (size (....))
		    (make-instance 'spatial-index ))))
   (make-instance 'spatial-index )))


;; octree node has two slots: 1. boundary vector  2. child node container
;; (defmethod make-oct-node ((tri triangle))
;;   )

;; (defmethod make-oct-node ((node list))
;;   (list nil (make-list 8)))

(defun find-bounds (array)
  (loop for vertex across array
     for x = (aref vertex 0)
     for y = (aref vertex 1)
     for z = (aref vertex 2)
     maximize x into max-x
     minimize x into min-x
     maximize y into max-y
     minimize y into min-y
     maximize z into max-z
     minimize z into min-z
     finally (return (values (list min-x min-y min-z)
			     (list max-x max-y max-z)))))

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

(defun bounded-by-p (vector1 vector2)
  (declare (type (vector single-float 6) vector1 vector2))
  "Tests whether VECTOR1 is bounded by VECTOR2"
  (and (every #'>= (subseq vector1 0 3) (subseq vector2 0 3))
       (every #'<= (subseq vector1 3) (subseq vector2 3))))

(defun bounds-p2 (vector1 vector2)
  (and (every #'>= (subseq vector1 0 3) (subseq vector2 3))
       (every #'< (subseq vector1 3) (subseq vector2 0 3))))

(defun subdivide (bound)
  "Subdivides a BOUND representing an AABB into 8 AABBes"
  (macrolet ((lxor (a b)
	       ;; needed a logical xor :(
	       (let ((aval (gensym))
		     (bval (gensym)))
		 `(let ((,aval ,a)
			(,bval ,b))
		    (and (or ,bval ,aval)
			 (not (and ,bval ,aval)))))))
    (loop :for s :below 8 :collect
       (coerce (loop :for j :below 6
		  :for m = (mod j 3) :collect
		  (if (lxor (not (zerop
				  (boole boole-and
					 (ash s (- m)) 1)))
			    (> j 2))
		      (* (+ (aref bound m)
			    (aref bound (+ m 3))) 0.5)
		      (aref bound j)))
	       '(vector single-float 6)))))

(defun make-octree (bounds list)
  (let ((max-depth (- +max-levels+ 1)))
    (labels ((low-tolerance-p (bound)
	       (< (- (aref bound 3) (aref bound 0))
		   (* +tolerance+ 4.0)))
	     (bind (bound list)
	       ;; collect all items in LIST within BOUND into a new list
	       (loop :for triangle :in list
		  :if (bounded-by-p (bound triangle) bound)
		  :collect it))
	     (make-octree-node (bounds list depth)
	       ;; build octree nodes recursively
	       (list bounds
		     (if (and (< depth max-depth)
			      (> (list-length list) *max-items*)
			      (not (low-tolerance-p bounds)))
			 (loop :for sub-bound :in (subdivide bounds)
			    :collect (make-octree-node sub-bound
						       (bind sub-bound list) 
						       (+ depth 1)))
			 list))))
      (make-octree-node bounds list 0))))

(defmethod make-spatial-index ((bounds aa-box) (node vector))
  (make-instance 'spatial-index :bounds bounds :nodes nodes))

(defmethod make-spatial-index ((bounds aa-box) (node triangle)))

(defmethod make-spatial-index ((bounds aa-box) (nodes spatial-index))
  (let ((max-depth (- +max-levels+ 1)))
    (labels ((low-tolerance-p (bounds)
	       (< (- (hx bounds) (lx bounds))
		  (* +tolerance+ 4.0)))
	     (bind (bounds vector)
	       ;; collect all items in LIST within BOUND into a new list
	       (coerce (loop :for item :across vector
			  :if (bounded-by-p (bound item) bounds)
			  :collect it)
		       'vector))
	     (make-spatial-node (bounds vector depth)
	       ;; build octree nodes recursively
	       (make-spatial-index bounds
				   (if (and (< depth max-depth)
					    (> (length vector) *max-items*)
					    (not (low-tolerance-p bounds)))
				       (loop :for sub-bound :in (subdivide bounds)
					  :collect (make-octree-node sub-bound
								     (bind sub-bound list) 
								     (+ depth 1)))
			 list))))
      (make-spatial-node bounds list 0))))
;;; ------------------- Intersection -------------------------------------------


(defun find-subcell (ray spatial-index)
  "Find which subcell contains ray origin")

;; (defmethod intersect-p ((ray ray) (nodes list))
;;   (some #'(lambda (object)
;; 	    (intersect-p ray object)) nodes))

;; (defmethod intersect-p ((ray ray) (node vector))
;;   (flet ((find-subcell ()
;; 	   (.....)))
;;     (multiple-value-bind (hit-item hit-position)
;; 	(intersect-p ray (find-subcell))
;;       (values hit-item hit-position))))

;; (defmethod intersect-p ((ray ray) (index spatial-index))
;;   (labels
;;       ((intersect-with-start (ray index last-hit start)
;; 	 (with-slots (origin direction) ray
;; 	   (with-slots (bound nodes) index
;; 	     (let* ((cell-position (or start origin))
;; 		    (start cell-position))
;; 	       (flet ((nfind-subcell (subcell start)
;; 			;; finds initial subcell. modifies SUBCELL
;; 			(loop
;; 			   :with center = (midpoint bound)
;; 			   :for i :from 3 :downto 0
;; 			   :do (setf subcell
;; 				     (boole boole-ior
;; 					    subcell (ash (if (>= (aref start i)
;; 								 (aref center i))
;; 							     1 0)
;; 							 i)))
;; 			   :finally (return subcell)))
;; 		      (nfind-next-subcell (subcell)
;; 			;; finds next subcell. modifies CELL-POSITION and SUBCELL
;; 			(let ((axis 2)
;; 			      (step (vec3)))
;; 			  (loop :for i :from 3 :downto 0
;; 			     :for high = (boole boole-and (ash subcell (- i)) 1)
;; 			     :for face = (if (lxor (plusp (aref direction i))
;; 						   high)
;; 					     (aref bound (+ i (* high 3)))
;; 					     (* (+ (aref bound i)
;; 						   (aref bound (+ i 3)))
;; 						0.5))
;; 			     :do (setf (aref step i)
;; 				       (/ (- face (aref origin i))
;; 					  (aref direction i))))
;; 			  (when (not (lxor (not (zerop (boole boole-and
;; 							      (ash subcell (- axis)) 1)))
;; 					   (plusp (aref direction axis))))
;; 			    (let ((new-cell-pos (vector+ origin
;; 							 (vector* direction
;; 								  (aref step axis))))
;; 				  (new-subcell (boole boole-xor
;; 						      subcell
;; 						      (ash 1 axis))))
;; 			      (values new-cell-pos new-subcell)))))
;; 		      (verify-distance ()
;; 			(....)))
;; 		 (let ((subcell (nfind-subcell 0 cell-position)))
;; 		  (multiple-value-bind (hit-item hit-position)
;; 		      (intersect-with-start ray
;; 					    (aref nodes subcell)
;; 					    last-hit
;; 					    cell-position)
;; 		    (if hit-item
;; 			(if (floatp hit-position)
;; 			    ;;must really be hit-distance  -- I don't understand this step.
;; 			    (verify-distance hit-position)
;; 			    (values hit-item hit-position))
;; 			(intersect-with-start ray (find-next-subcell subcell)))))))))))
;;     (intersect-with-start ray index nil nil)))

;; (defun intersect-recurser (ray-origin ray-direction last-hit start)
;;   )



;; (defmethod intersect-p ((ray ray) (octree list)) ;needs to be spatial-index
;;   (labels
;;       ((intersect-from-start (ray octree last-hit start)
;; 	 (with-slots (origin direction) ray
;; 	   (with-slots (bounds nodes) octree
;; 	     (let ((start (or start origin))
;; 		   hit-object
;; 		   hit-position
;; 		   (item-ps (make-array (length items))))
;; 	       (if (spatial-index-p (first octree))
;; 		   (let ((midpoint (midpoint origin direction))
;; 			 (sub-cell (find-subcell ray octree)))
;; 		     (multiple-value-bind (hit-object hit-position)
;; 			 (intersect-from-start ray (aref nodes sub-cell) last-hit start)))
;; 		   ;; else:
;; 		   (loop
;; 		      :with nearest-distance = most-positive-fixnum
;; 		      :for item :across nodes
;; 		      :do
;; 		      (let ((distance (interect-p ray item)))
;; 			(when (and distance (< distance nearest-distance))
;; 			  (let ((hit (vector+ origin
;; 					      (vector* direction distance))))
;; 			    (flet ((within-tolerance-p (vec1 vec2)
;; 				     (flet ((<=tolerance (x) (<= x +tolerance+)))
;; 				       (and (every #'<=tolerance
;; 						   (vector- (subseq vec2 0 3)))
;; 					    (every #'<=tolerance
;; 						   (vector- vec1 (subseq vec2 3)))))))
;; 			      (when (within-tolerance-p hit bound)
;; 				(setf hit-object item
;; 				      hit-position hit
;; 				      nearest-distance distance))))))))
;; 	       (values hit-object hit-position))))))
    
;;     (intersect-from-start ray octree nil nil)))


;; (defmethod intersect-p ((ray slope-ray) (index spatial-index))
;;   ;; Intersection test between ray and octree
;;   ;; TODO: determine if a FIND-SUBCELL function is needed. For now, leave out.
;;   (with-slots (bounds nodes) index
;;     (with-slots (origin direction) ray
;;       (let ((distance (intersect-p ray bounds)))
;; 	(when distance
;; 	  (let ((hit (hit-point ray distance)))
;; 	    ;; Maybe pull this out into the triangle intersect code.
;; 	    (when (and (in-bounds-p hit) )
;; 	      (intersect-p ray nodes))))))))


(defmethod intersect-p ((ray slope-ray) (index spatial-index))
  ;; Intersection test between ray and octree
  ;; TODO: determine if a FIND-SUBCELL function is needed. For now, leave out.
  (with-slots (bounds nodes) index
    (with-slots (origin direction) ray
      (let ((distance (intersect-p ray bounds)))
	(when distance
	  (intersect-p ray nodes))))))


(defmethod intersect-p ((ray slope-ray) (nodes vector))
  ;; Test each node until a hit.
  (some #'intersect-p nodes))
