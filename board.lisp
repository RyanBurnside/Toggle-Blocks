(in-package #:puzzle)

;; Note, when setfing the blocks you need to (setf (aref (blocks <foo>) Y X) <value>)

(defclass board ()
  ((height :initarg :height :initform 13 :accessor height)
   (width :initarg :width :initform 7 :accessor width)
   (current-piece :initarg :current-piece :initform nil :accessor current-piece)
   (blocks :initarg :blocks :initform (make-array `(1 1)) :accessor blocks)))

(defun make-board (rows columns)
  (make-instance 'board
		 :height rows
		 :width columns
		 :current-piece nil
		 :blocks (make-array `(,rows ,columns))))

(defmethod board-rows ((board board))
	   (array-dimensions (blocks board) 0))

(defmethod board-columns ((board board))
	   (array-dimensions (blocks board) 1))

(defmethod isolate-groups ((board board) group-size)
  ;; Duplicate the board
  ;; Create a temp called queue
  ;; Create a temp called group
  ;; Create a temp called groups
  ;; For each position p in the board
  ;; If p has a cell (not nil) 
  ;;    Move the cell to the group
  ;;    Set the cell's location to NIL
  ;;      Add the non NIL neighbors to the queue
  ;;      if queue is not empty, pop off a new P
  ;;      if the queue is empty and group is >= required sice
  ;;         push group onto groups
  ;;         exit to next position)
  ;; Finally return groups
  )
             
