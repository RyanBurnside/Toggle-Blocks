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

(defmacro with-board ((row-sym col-sym board) &body body)
  (a:once-only (board)
    `(dotimes (,row-sym (board-rows ,board))
       (dotimes (,col-sym (board-columns ,board))
         ,@body))))

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

  (let* (;; markedvisited array keeps track of visited cells (boolean)
         (visited (make-array (array-dimensions (blocks board))
                              :initial-element nil))
         groups ; Return result
         start-piece ; The "seed" per row/col iteration
         queue ; used in BFS
         group ; collects current matching piece-blocks
         current)

    ;; Scan the board
    (with-board (row column board)
      (setf start-piece (aref (blocks board) row column))
      ;; Start a new group if piece exists and hasn't been visited
      (when (and start-piece (null (aref visited row column)))
        (setf queue (list start-piece)
              group nil)

        ;; Mark starting cell visited immediately
        (setf (aref visited row column) t)

        ;; BFS loop (builds up group)
        (loop while queue do
          (setf current (pop queue))
          (push current group)

          ;; Examine neighbors
          (dolist (neighbor (get-neighbors board (y current) (x current) 8-way-p))
            ;; Only consider neighbors of same color that haven't been visited
            (when (and (typep neighbor 'piece-block)
                       (= (color1 neighbor) (color1 current))
                       (null (aref visited (y neighbor) (x neighbor))))
              (setf (aref visited (y neighbor) (x neighbor)) t)
              (push neighbor queue))))

        ;; Save group if it meets minimum size
        (when (>= (length group) group-size)
          (push group groups))))

      ;; Return all valid groups
      groups))

(defmethod delete-groups ((board board) size)
  "Deletes the current groups, doesn't move pieces down."
  (dolist (group (isolate-groups board size))
    (dolist (block-piece group)
      (setf (aref (blocks board)
                  (y block-piece)
                  (x block-piece))
            nil))))


;; TODO monitor the row value change and return number of changes
(defmethod compress-column-down ((board board) col)
  "Takes a column number and pushes all pieces down to the bottom
removing all gaps (nil)s. Returns t if things shifted."
  ;; Collect only the solids in the column
  (let ((solids (loop for row from 0 below (board-rows board)
                      for value = (aref (blocks board) row col)
                      when value
                        collect value
                      do (setf (aref (blocks board) row col) nil)))
        moved-p)

    ;; Move from bottom up setting the caught values
    (loop for value in (reverse solids)
          for row from (1- (board-rows board)) downto 0
          when (/= (y value) row)
            do (setf moved-p t)
          do (setf (x value) col
                   (y value) row)
             (setf (aref (blocks board) row col) value))
    moved-p))

;; TODO Return t if movement happened in the rows so it can be called until nil
(defmethod compress-blocks-down ((board board))
  "Shifts all blocks down, returns t if something moved."
  (let (moved-p)
    (dotimes (col (board-columns board))
      (when (compress-column-down board col)
        (setf moved-p t)))
    moved-p))
