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

(defun isolate-groups ((board board) group-size)
  (let* (found-groups
	 (board-blocks (blocks-board)) ;; Real board 
	 (temp-array (make-array (array-dimensions board-blocks))))
    (replace temp-array (blocks board)) ;; Shallow copy array
    (dotimes* (row (board-rows temp-array)
		   column (board-columns temp-array))
	      
	      
			   
