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
  (first (array-dimensions (blocks board))))

(defmethod board-columns ((board board))
  (second (array-dimensions (blocks board))))

(defmethod inside-board-p ((board board) row column)
  (and (<= 0 row (1- (board-rows board)))
       (<= 0 column (1- (board-columns board)))))
       
(defmethod get-neighbors ((board board) row col &optional 8-way-p &aux results)
  "Return a list of neighbors of a cell. 4 way unless 8-way-p is T.
access beyond borders is allowed but ignored."
  (flet ((maybe-collect (r c)
	   (let ((new-row (+ r row))
		 (new-column (+ c col)))
	     (when (inside-board-p board new-row new-column)
	       (push (aref (blocks board) new-row new-column)
		     results)))))
    (maybe-collect  1  0)
    (maybe-collect -1  0)
    (maybe-collect  0  1)
    (maybe-collect  0 -1)
    (when 8-way-p
      (maybe-collect -1 -1)
      (maybe-collect  1 -1)
      (maybe-collect  1  1) 
      (maybe-collect -1  1))
    results))

(defmethod isolate-groups ((board board) group-size &optional 8-way-p)
  "Finds all contiguous groups of pieces on BOARD of at least GROUP-SIZE.
8-WAY-P controls whether diagonal neighbors are considered."

  (let* ((groups nil)
         ;; Painted array keeps track of visited cells (boolean)
         (painted (make-array (array-dimensions (blocks board))
                              :initial-element nil)))

    ;; Scan the board
    (dotimes (row (board-rows board))
      (dotimes (column (board-columns board))
        (let ((start-piece (aref (blocks board) row column)))
          ;; Start a new group if piece exists and hasn't been visited
          (when (and start-piece (null (aref painted row column)))
            (let ((queue (list start-piece))
                  (group nil))
              
              ;; Mark starting cell visited immediately
              (setf (aref painted row column) t)

              ;; BFS loop
              (loop while queue do
                (let ((current (pop queue)))
                  (push current group)

                  ;; Examine neighbors
                  (dolist (neighbor (get-neighbors board (y current) (x current) 8-way-p))
                    ;; Only consider neighbors of same color that haven't been visited
                    (when (and (= (color1 neighbor) (color1 current))
                               (null (aref painted (y neighbor) (x neighbor))))
                      (setf (aref painted (y neighbor) (x neighbor)) t)
                      (push neighbor queue)))))

              ;; Save group if it meets minimum size
              (when (>= (length group) group-size)
                (push group groups)))))))

    ;; Return all valid groups
    groups))
