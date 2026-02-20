(in-package #:puzzle)

(defclass animation ()
  ((start :initarg start :initform (cons 0 0) :accessor start)
   (end :initarg end :initform (cons 0 0) :accessor end)
   (u :initform 0 :reader u)
   (total-steps :initarg :total-steps :initform 5 :accessor :total-steps)
   (step :initarg :step :initform 0 :accessor :step)
   (func :initarg :func :initform #'linear-interpolate))) 

