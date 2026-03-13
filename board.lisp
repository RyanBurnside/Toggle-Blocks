(in-package #:puzzle)

(defclass board ()
  ((height :initarg :height :initform 13 :accessor height)
   (width :initarg :width :initform 7 :accessor width)
   (current-piece :initarg :current-piece :initform nil :accessor current-piece)
   (blocks :initarg :blocks :initform (make-array `(1 1)) :accessor blocks)
   (animations :initform nil :accessor animations)
   (state :initform 'starting)))

;; Group Extraction
;; Group Removal
;;   -> Destruction animations/sound loop
;; Shift Down (save current board, shift, form interpolations)
;;   -> Interpolation animations/sound loop

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

(defmethod piece-at ((board board) row col)
  (aref (blocks board) row col))

(defmethod set-at ((board board) row col value)
  (seta (blocks board) (row col) value))

(defmethod inside-board-p ((board board) row column)
  (and (<= 0 row (1- (board-rows board)))
       (<= 0 column (1- (board-columns board)))))

(defmacro do-board ((row-sym col-sym board) &body body)
  (a:once-only (board)
    `(dotimes (,row-sym (board-rows ,board))
       (dotimes (,col-sym (board-columns ,board))
         ,@body))))

(defmethod get-neighbors ((board board) row col)
  "Return a list of neighbors of a cell.
access beyond borders is allowed but ignored."
  (loop with adjacent = '((1 0) (-1 0) (0 1) (0 -1))
        for (r c) in adjacent
        for new-row = (+ r row)
        for new-column = (+ c col)
        when (inside-board-p board new-row new-column)
          collect (piece-at board new-row new-column)))

(defmethod isolate-groups ((board board) group-size)
  "Finds all contiguous groups of pieces on BOARD of at least GROUP-SIZE."

  (let* (;; markedvisited array keeps track of visited cells (boolean)
         (visited (make-array (array-dimensions (blocks board))
                              :initial-element nil))
         groups ; Return result
         start-piece ; The "seed" per row/col iteration
         queue ; used in BFS
         group ; collects current matching piece-blocks
         current)

    ;; Scan the board
    (do-board (row column board)
      (setf start-piece (piece-at board row column))
      ;; Start a new group if piece exists and hasn't been visited
      (when (and start-piece (null (aref visited row column)))
        (setf queue (list start-piece)
              group nil)

        ;; Mark starting cell visited immediately
        (seta visited (row column) t)

        ;; BFS loop (builds up group)
        (loop while queue do
          (setf current (pop queue))
          (push current group)

          ;; Examine neighbors
          (dolist (neighbor (get-neighbors board (y current) (x current)))
            ;; Only consider neighbors of same color that haven't been visited
            (when (and (typep neighbor 'piece-block)
                       (= (color1 neighbor) (color1 current))
                       (null (aref visited (y neighbor) (x neighbor))))
              (seta visited ((y neighbor) (x neighbor)) t)
              (push neighbor queue))))

        ;; Save group if it meets minimum size
        (when (>= (length group) group-size)
          (push group groups))))

    ;; Return all valid groups
    groups))

;; TODO make this set-piece-f at end
(defmethod delete-groups ((board board) size)
  "Deletes the current groups, doesn't move pieces down."
  (dolist (group (isolate-groups board size))
    (dolist (block-piece group)
      (set-at board
              (y block-piece)
              (x block-piece)
              nil))))


(defmethod compress-column-down ((board board) col)
  "Takes a column number and pushes all pieces down to the bottom
removing all gaps (nil)s. Returns t if things shifted. this function also
pushes animations into the board's animations slot. Block animations are row, column"
  ;; Algorithm
  ;; Collect pieces while clear position (column eventually gets blanked)

  (let ((solids (loop for row from 0 below (board-rows board)
                      for value = (piece-at board row col)
                      when value
                        collect value
                      do (set-at board row col nil)))
        moved-p)

    ;; Move from bottom up setting the piece values
    (loop for piece in (reverse solids)
          for row from (1- (board-rows board)) downto 0
          when (/= (y piece) row)
            do (setf moved-p t)
          do ; Push new animation here
             (push (make-block-animation piece
                                         `(,(row piece) ,col)
                                         `(,row ,col)
                                         10)
                   (animations board))
             (setf (y piece) row)
             (set-at board row col piece))
    moved-p))

;; TODO Return t if movement happened in the rows so it can be called until nil
(defmethod compress-blocks-down ((board board))
  "Shifts all blocks down, returns t if something moved."
  (let (moved-p)
    (dotimes (col (board-columns board))
      (when (compress-column-down board col)
        (setf moved-p t)))
    moved-p))
