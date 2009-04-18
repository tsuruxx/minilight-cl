(defclass aa-bbox ()
  ((lx :initarg :lx :type single-float)
   (ly :initarg :ly :type single-float)
   (lz :initarg :lz :type single-float)
   (hx :initarg :hx :type single-float)
   (hy :initarg :hy :type single-float)
   (hz :initarg :hz :type single-float)))

(defun make-aa-bbox (lx ly lz hx hy hz)
  (when (and (< lx hx) (< ly hy) (< lz hz))
    (make-instance 'aa-bbox
		   :lx lx :ly ly :lz lz
		   :hx hx :hy hy :hz hz)))

(defmethod subdivide ((box aa-bbox))
  (with-slots (lx ly lz hx hy hz) box
    (macrolet ((lxor (a b)
		;; needed a logical xor :(
		(let ((aval (gensym))
		      (bval (gensym)))
		  `(let ((,aval ,a)
			 (,bval ,b))
		     (and (or ,bval ,aval)
			  (not (and ,bval ,aval)))))))
      (multiple-value-bind (mid-x mid-y mid-z)
	  (center box)
	(loop :for s :below 8 :collect
	   (apply #'make-aa-box
		  (loop :for j :below 6
		     :for m = (mod j 3) :collect
		     (if (lxor (not (zerop
				     (boole boole-and
					    (ash s (- m)) 1)))
			       (> j 2))
			 (* (+ (aref bound m)
			       (aref bound (+ m 3))) 0.5)
			 (aref bound j)))))))))
(defmethod center ((box aa-box))
  (with-slots (lx ly lz hx hy hz) box
    (let ((mid-x (- hx lx))
	  (mid-y (- hy ly))
	  (mid-z (- hz lz)))
      (values (* mid-x 0.5) (* mid-y 0.5) (* mid-z 0.5)))))

(defun subdivide (bound)
  "Subdivides a BOUND representing an AABB into 8 AABBes"
  )