(in-package #:puzzle)

(defclass animation ()
  ((start :initarg :start :initform (list 0 0) :accessor start)
   (end :initarg :end :initform (list 0 0) :accessor end)
   (total-steps :initarg :total-steps :initform 5 :accessor total-steps)
   (step :initarg :step :initform 0 :accessor step)
   (func :initarg :func :initform #'linear-interpolate :accessor func)
   (repeat :initarg :repeat :initform nil :accessor repeat)))

(defmethod update ((a animation))
  "Updates an animation's position"
  (with-accessors ((step step)
                   (total-steps total-steps)) a
    (cond ((< step (total-steps a)) ; Update if less than
           (incf step))
          ((repeat a) ; Knock back to 0 on repeat
           (setf step 0)))))

(defmethod u ((a animation))
  "Finds the position along the animation given its current state."
  (mapcar (lambda (s e)
            (funcall (func a) s e (/ (step a) (total-steps a))))
            (start a) (end a)))
