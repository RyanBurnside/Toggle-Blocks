(in-package #:puzzle)

(defclass coord () ; coord mixin
  ((x :initarg :x :initarg :column
      :initform 0
      :accessor x :accessor column)
   (y :initarg :y :initarg :row
      :initform 0
      :accessor y :accessor row)))

(defmethod move ((c coord) &key row column x y coord)
  (when row (setf (row c) row))
  (when column (setf (column c) column))
  (when x (setf (x c) x))
  (when y (setf (y c) y))
  (when coord
    (setf (row c) (row coord)
          (column c) (column coord))))

(defclass piece-block (coord)
  ((color1 :initarg :color1 :accessor color1)
   (color2 :initarg :color2 :accessor color2)
   (marked :initform nil :accessor marked)))

;; Note, color2 is used for toggling next color
;; color1 is used for matching

;; TODO this x y crap needs to become column row
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
  ((blocks :initarg :blocks :initform nil :accessor blocks)))

(defun make-random-3piece (x y color-list)
  (let ((temp (make-instance 'piece :x x :y y))
        (offsets (if (zerop (random 2))
                     `((-1 . 0) (0 . 0) (1 .  0)) ; hori line
                     `((0 . -1) (0 . 0) (1 . 0))))) ; L shape
    (loop for (xx . yy) in offsets
          do (push (make-random-block (+ xx x) (+ yy y) color-list)
                   (blocks temp)))
    temp))
