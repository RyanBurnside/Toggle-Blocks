(in-package #:puzzle)

(defclass coord () ; coord mixin
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defclass piece-block (coord)
  ((color1 :initarg :color1 :accessor color1)
   (color2 :initarg :color2 :accessor color2)))

(defun make-block (x y color1 color2)
  (make-instance 'piece-block :x x
			      :y y
			      :color1 color1
			      :color2 color2))

(defun make-random-block (x y color-list)
  (make-block x
	      y
	      (nth (random (length color-list)) color-list)
	      (nth (random (length color-list)) color-list)))
   
(defclass piece (coord)
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)
   (blocks :initarg :blocks :initform nil :accessor blocks)))

(defun make-random-3piece (x y color-list)
  (let ((temp (make-instance 'piece :x x :y y))
	(offsets (if (zerop (random 2))
		     `((-1 . 0) (0 . 0) (1 .  0)) ; hori line
		     `((0 . -1) (0 . 0) (1 . 0))))) ; L shape
    (loop for (xx . yy) in offsets
	  do (push (make-random-block (+ xx x) (+ yy y) color-list)
		   (blocks temp)))
    temp))

      
      
  
