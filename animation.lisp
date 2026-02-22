(in-package #:puzzle)

;;;; Animations are kind of an abstract things
;;;; they provide movement from start to finish
;;;; Start and end may be a coordinate list of any number


(defclass animation ()
  ((start :initarg :start :initform (list 0 0) :accessor start)
   (end :initarg :end :initform (list 0 0) :accessor end)
   (total-steps :initarg :total-steps :initform 5 :accessor total-steps)
   (steps :initarg :steps :initform 0 :accessor steps)
   (func :initarg :func :initform #'linear-interpolate :accessor func)
   (repeat :initarg :repeat :initform nil :accessor repeat)))

;; block-animation uses a piece-block reference but ignores row,column
(defclass block-animation (animation)
  ((piece-block :initarg :piece-block :initform nil :accessor piece-block)))

(defun make-block-animation (piece-block start end total-steps)
  "Some minimal constructor. Assumes repeat nil and #'linear-interpolate"
  (make-instance 'block-animation
                 :start start
                 :end end
                 :total-steps total-steps
                 :piece-block piece-block))

(defmethod update ((a animation))
  "Updates an animation's position"
  (with-accessors ((steps steps)
                   (total-steps total-steps)) a
    (cond ((< steps total-steps) ; Update if less than
           (incf steps))
          ((repeat a) ; Knock back to 0 on repeat
           (setf steps 0)))))

(defmethod current-position ((a animation))
  "Finds the position along the animation given its current state."
  (mapcar (lambda (s e)
            (funcall (func a) s e (/ (steps a) (total-steps a))))
          (start a) (end a)))
