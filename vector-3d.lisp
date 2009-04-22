;; 
;;
;; Copyright (c) 2008, Charlie McMackin
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;   1. Redistributions of source code must retain the above copyright notice,
;;      this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above copyright notice,
;;      this list of conditions and the following disclaimer in the documentation
;;      and/or other materials provided with the distribution.
;;   3. The name of the author may not be used to endorse or promote products
;;      derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
;; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
;; EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;; IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;


;;; Intended for common 3D vector math functions
;;; started 2008-3-25 Charlie McMackin

(defpackage #:vector-3d
  (:nicknames #:v3d)
  (:use #:common-lisp)
  (:export #:vector3d #:vector4d 
           #:vec3-0 #:vec3 #:distance
           #:distance^2 #:vector+ #:vector-
           #:nvector+ #:nvector- #:vector* #:nvector*
           #:vector-zerop #:vec3-max #:vec3-min
           #:cross #:dot #:normalize #:nnormalize
           #:mag-sq #:magnitude #:unit-normal-tri #:nunit-normal-tri
           #:determinant #:inverse #:ninverse #:vec4-0 #:vec4
           #:clamp #:vector-clamp #:nvector-clamp))


(in-package #:vector-3d)

#+nil(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))
(declaim (optimize (speed 3)))

(deftype vector3d () '(simple-array single-float (3)))
(deftype vector4d () '(simple-array single-float (4)))
(declaim (inline vector+ nvector+ vector- nvector- vector* nvector*
                 dot mag-sq magnitude vec3-0 vec3 vec4-0 vec4))

(defun vec3-0 () (make-array 3 :element-type 'single-float :initial-element 0.0))
(defun vec3-max () (make-array 3 :element-type 'single-float
                               :initial-element most-positive-single-float))
(defun vec3-min () (make-array 3 :element-type 'single-float
                               :initial-element most-negative-single-float))
(defun vec3 (x y z)
  "Return a 3 element array with X Y Z single-float elements"
  (declare (number x y z))
  (let ((vec (vec3-0)))
    (setf (aref vec 0) (coerce x 'single-float)
          (aref vec 1) (coerce y 'single-float)
          (aref vec 2) (coerce z 'single-float))
    vec))

(defun vec4-0 () (make-array 4 :element-type 'single-float :initial-element 0.0))
(defun vec4 (x y z w)
  "Return a 4 element array with X Y Z W single-float elements"
  (let ((vec (vec4-0)))
    (setf (aref vec 0) (float x 1f0)
          (aref vec 1) (float y 1f0)
          (aref vec 2) (float z 1f0)
          (aref vec 3) (float w 1f0))
    vec))

#+ignore
(defun distance (seq1 seq2)
  (sqrt (+ (expt (- (elt seq1 0) (elt seq2 0)) 2)
           (expt (- (elt seq1 1) (elt seq2 1)) 2)
           (expt (- (elt seq1 2) (elt seq2 2)) 2))))

(defun distance^2 (seq1 seq2)
  "Return the squared distance of two 3-space vectors"
  (let ((seq3 (vector- seq1 seq2)))
    (dot seq3 seq3)))

(defun distance (seq1 seq2)
  "Return distance between two 3-space vectors"
  (declare (type vector3d seq1 seq2))
  (sqrt (+ (expt (- (aref seq1 0) (aref seq2 0)) 2)
           (expt (- (aref seq1 1) (aref seq2 1)) 2)
           (expt (- (aref seq1 2) (aref seq2 2)) 2))))

(defun midpoint (seq1 seq2)
  "Return the midpoint between two 3-space vectors"
  (declare (type vector3d seq1 seq2))
  (map 'vector3d #'(lambda (x y) 
                     (declare (single-float x y))
                     (* 0.5 (+ x y)))
       seq1 seq2))

(defun bisect (ray)
  "Returns 2 rays of "
  ;;ray is a 2 element list of vector3d
  (let ((mpt (apply #'midpoint ray)))
    (values (list (first ray) mpt)
            (list mpt (second ray)))))

#+ignore
(defun vector+ (seq1 seq2)
  (vector (+ (elt seq1 0) (elt seq2 0))
          (+ (elt seq1 1) (elt seq2 1))
          (+ (elt seq1 2) (elt seq2 2))))

(defun %vector+ (seq1 seq2)
  "Add two 3-space vectors"
  (declare (type vector3d seq1 seq2))
  (vec3 (+ (aref seq1 0) (aref seq2 0))
        (+ (aref seq1 1) (aref seq2 1))
        (+ (aref seq1 2) (aref seq2 2))))

(defun vector+ (&rest args)
  "Add all vectors in ARGS"
  (reduce #'%vector+ args))

(defun nvector+ (vec1 vec2)
  "Add two 3-space vectors destructively modifying the first"
  (declare (type vector3d vec1 vec2))
  (map-into vec1 #'+ vec1 vec2))

#+ignore
(defun vector- (seq1 seq2)
  (vector (- (elt seq1 0) (elt seq2 0))
          (- (elt seq1 1) (elt seq2 1))
          (- (elt seq1 2) (elt seq2 2))))

(defun vector- (seq1 &optional seq2)
  "Shange the signs of every element or subtract two 3-space vectors"
  (declare (type vector3d seq1))
  (if seq2
      (locally
          (declare (type vector3d seq2))
        (vec3 (- (aref seq1 0) (aref seq2 0))
              (- (aref seq1 1) (aref seq2 1))
              (- (aref seq1 2) (aref seq2 2))))
      (map 'vector3d #'- seq1)))

(defun nvector- (vec1 &optional vec2)
  "Subtract two 3-space vectors destructively modifying the first"
  (declare (type vector3d vec1))
  (if vec2
      (locally (declare (type vector3d vec2))
        (map-into vec1 #'- vec1 vec2))
      (map-into vec1 #'- vec1)))

(defun vector*scalar (seq1 scalar)
  "Multiply vector SEQ1 by a scalar SCALAR"
  (declare (type vector3d seq1) (single-float scalar))
  (map 'vector3d #'(lambda (x) (declare (single-float x))
                           (* x scalar)) 
       seq1))

(defun vector*vector (seq1 seq2)
  "Multiply vector SEQ1 to vector SEQ2"
  (declare (type vector3d seq1 seq2))
  (map 'vector3d #'* seq1 seq2))

(defun nvector*scalar (seq1 scalar)
  "Desctructively multiply vector SEQ1 by a scalar SCALAR"
  (declare (type vector3d seq1) (single-float scalar))
  (map-into seq1 #'(lambda (x) (declare (single-float x))
                           (* x scalar)) 
            seq1))

(defun nvector*vector (seq1 seq2)
  "Desctructively multiply vector SEQ1 by SEQ2"
  (declare (type vector3d seq1 seq2))
  (map-into seq1 #'* seq1 seq2))

(defun vector* (a b)
  "Multiply vector A by B where B may be a scalar or vector"
  (typecase b
    (float (vector*scalar a b))
    (vector3d (vector*vector a b))))

(defun nvector* (a b)
  "Destructively multiply vector A by B where B may be a scalar or vector"
  (typecase b
    (float (nvector*scalar a b))
    (vector3d (nvector*vector a b))))


(defun midpoint2 (seq1 seq2)
  (declare (type vector3d seq1 seq2))
  (nvector* (vector+ seq1 seq2) 0.5))

(defun cross (seq1 seq2)
  "Return the cross product between two 3-space vectors, SEQ1 and SEQ2"
  (declare (type vector3d seq1 seq2))
  (flet ((cp (i j)
           (- (the single-float (* (aref seq1 i) (aref seq2 j)))
              (the single-float (* (aref seq2 i) (aref seq1 j))))))
    (vec3 (cp 1 2) (cp 2 0) (cp 0 1))))

#+ignore   
(defun cross2 (a b)
  ;; from SLIK
  "cross a b

Computes and returns the cross-product of two vectors a and b  -- each
vector is int the form (x y z)."

  ;; If  A = a1i+a2j+a3k and B = b1i+b2j+b3k,
  ;; then the cross product of A and B , AxB , is defined by
  ;; AxB = (a2b3-a3b2)i+(a3b1-a1b3)j+(a1b2-a2b1)k.

  (let ((a1 (first a))
        (a2 (second a))
        (a3 (third a))
        (b1 (first b))
        (b2 (second b))
        (b3 (third b)))
    (list (- (* a2 b3) (* a3 b2))
          (- (* a3 b1) (* a1 b3))
          (- (* a1 b2) (* a2 b1)))))
#+ignore
(defun dot (seq1 seq2)
  (+ (* (elt seq1 0) (elt seq2 0))
     (* (elt seq1 1) (elt seq2 1))
     (* (elt seq1 2) (elt seq2 2))))

(defun dot (seq1 seq2)
  "Return the dot product of two 3-space vectors, SEQ1 and SEQ2"
  (declare (type vector3d seq1 seq2))
  (+ (* (aref seq1 0) (aref seq2 0))
     (* (aref seq1 1) (aref seq2 1))
     (* (aref seq1 2) (aref seq2 2))))

#+ignore
(defun dot (seq1 seq2)
  (reduce #'+ 
          (map 'list #'* seq1 seq2)))

(defun mag-sq (vector-3)
  "Return magnitude squared (magnitude^2) of VECTOR-3"
  (declare (type vector3d vector-3))
  (+ (expt (aref vector-3 0) 2)
     (expt (aref vector-3 1) 2)
     (expt (aref vector-3 2) 2)))

(defun magnitude (vector-3)
  "Return the magnitude or vector length of VECTOR-3"
  (declare (type vector3d vector-3))
  (let ((mag-squared (mag-sq vector-3)))
    (if (minusp mag-squared)
        0.0
        (sqrt mag-squared))))

#+ignore
(defun mag2 (seq)
  ;;this conses quite a lot versus magnitude
  (sqrt (reduce #'+ 
                (map 'vector #'(lambda (x) (expt x 2))
                     seq))))
(defun normalize (seq1)
  "Return the normalized length of SEQ1"
  (declare (type vector3d seq1))
  (let ((len (abs (magnitude seq1))))
    (cond ((= len 1.0) seq1)
          ((zerop len) (vec3-0))
          (t (vector* seq1 (/ 1.0 len))))))

(defun nnormalize (seq1)
  "Destructively modify SEQ1 to be its normalized length"
  (declare (type vector3d seq1))
  (let ((len (abs (magnitude seq1))))
    (cond ((= len 1.0) seq1)
          ((zerop len) (vec3-0))
          (t (nvector* seq1 (/ 1.0 len))))))

(defun unit-normal-tri (seq1 seq2 seq3)
  "Return the unit normal of three 3-space vectors SEQ1 SEQ2 and SEQ3"
  (declare (type vector3d seq1 seq2 seq3))
  (symbol-macrolet ((a (vector- seq1 seq2))
                    (b (vector- seq2 seq3)))
    (normalize (cross a b))))

(defun nunit-normal-tri (seq1 seq2 seq3)
  "Return the unit normal of three 3-space vectors SEQ1 SEQ2 and SEQ3"
  (declare (type vector3d seq1 seq2 seq3))
  (symbol-macrolet ((a (vector- seq1 seq2))
                    (b (vector- seq2 seq3)))
    (nnormalize (cross a b))))

(defun determinant (vec1 vec2 vec3)
  "Return the determinate of three 3-space vectors representing a column-major
 3x3 matrix"
  ;; from wikipedia definition but very ugly
  ;; assumed vec1,2,3 are column vectors                                                                  
  (declare (type vector3d vec1 vec2 vec3))
  (let* ((a (aref vec1 0))
         (b (aref vec2 0))
         (c (aref vec3 0))
         (d (aref vec1 1))
         (e (aref vec2 1))
         (f (aref vec3 1))
         (g (aref vec1 2))
         (h (aref vec2 2))
         (i (aref vec3 2)))
    (- (+ (* a e i) (* b f g) (* c d h))
       (+ (* b d i) (* a f h) (* c e g)))))

(defun inverse (vec)
  "Return the inverse vector of 3-space vector VEC"
  (declare (type vector3d vec))
  (map 'vector3d
       #'(lambda (x) 
           (declare (single-float x))  
           (if (= 0.0 x) 
               0.0 
               (/ 1.0 x)))
       vec))

(defun ninverse (vec)
  "Destructively invert 3-space vector VEC"
  (declare (type vector3d vec))
  (map-into vec 
            #'(lambda (x) 
                (declare (single-float x)) 
                (when (/= 0.0 x) 
                  (/ 1.0 x)))
            vec))

(defun random-vector (n)
  "Return a 3-space vector of random numbers in the range of N"
  (vec3 (random n) (random n) (random n)))

(defun random-list (n)
  "Return a 3 element list of random numbers in the range of N"
  (list (random n) (random n) (random n)))

(defun vector-zerop (vector)
  (declare (type vector vector))
  (every #'zerop vector))

;; From ALEXANDRIA http://www.common-lisp.net/project/alexandria
(declaim (inline clamp))
(defun clamp (number min max)
  "Clamps the NUMBER into [MIN, MAX] range. Returns MIN if NUMBER lesser then
MIN and MAX if NUMBER is greater then MAX, otherwise returns NUMBER."
  (if (< number min)
      min
      (if (> number max)
          max
          number)))

(defun vector-clamp (vector min-vector max-vector)
  (map 'vector #'clamp vector min-vector max-vector))

(defun nvector-clamp (vector min-vector max-vector)
  (map-into vector #'clamp vector min-vector max-vector))