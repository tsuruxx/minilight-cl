(in-package #:minilight)

(defclass aa-bbox ()
  ((lx :initarg :lx :reader lx :type single-float)
   (ly :initarg :ly :reader ly :type single-float)
   (lz :initarg :lz :reader lz :type single-float)
   (hx :initarg :hx :reader hx :type single-float)
   (hy :initarg :hy :reader hy :type single-float)
   (hz :initarg :hz :reader hz :type single-float)))

(defun make-aa-bbox (lx ly lz hx hy hz)
  (when (and (< lx hx) (< ly hy) (< lz hz))
    (make-instance 'aa-bbox
                   :lx lx :ly ly :lz lz
                   :hx hx :hy hy :hz hz)))

(defun vec-aa-bbox (bound-vec)
  (make-aa-bbox (aref bound-vec 0)
                (aref bound-vec 1)
                (aref bound-vec 2)
                (aref bound-vec 3)
                (aref bound-vec 4)
                (aref bound-vec 5)))

;; (defmethod subdivide ((box aa-bbox))
;;   (with-slots (lx ly lz hx hy hz) box
;;     (macrolet ((lxor (a b)
;;              ;; needed a logical xor :(
;;              (let ((aval (gensym))
;;                    (bval (gensym)))
;;                `(let ((,aval ,a)
;;                       (,bval ,b))
;;                   (and (or ,bval ,aval)
;;                        (not (and ,bval ,aval)))))))



;;       (loop :for s :below 8 :collect
;;       (apply #'make-aa-box
;;              (loop :for j :below 6
;;                 :for m = (mod j 3) :collect
;;                 (if (lxor (not (zerop
;;                                 (boole boole-and
;;                                        (ash s (- m)) 1)))
;;                           (> j 2))
;;                     (* (+ (aref bound m)
;;                           (aref bound (+ m 3))) 0.5)
;;                     (aref bound j))))))))


(defmethod center ((box aa-bbox))
  (with-slots (lx ly lz hx hy hz) box
    (let ((mid-x (+ hx lx))
          (mid-y (+ hy ly))
          (mid-z (+ hz lz)))
      (values (* mid-x 0.5) (* mid-y 0.5) (* mid-z 0.5)))))

(defmethod subdivide ((box aa-bbox))
  (with-slots (lx ly lz hx hy hz) box
    (multiple-value-bind (cx cy cz)
        (center box)
      ;; TODO: make generation programmatic
      (list (make-aa-bbox lx ly lz cx cy cz)
            (make-aa-bbox cx ly lz hx cy cz)
            (make-aa-bbox lx cy lz cx hy cz)
            (make-aa-bbox cx cy lz hx hy cz)
            
            (make-aa-bbox lx ly cz cx cy hz)
            (make-aa-bbox cx ly cz hx cy hz)
            (make-aa-bbox lx cy cz cx hy hz)
            (make-aa-bbox cx cy cz hx hy hz)))))