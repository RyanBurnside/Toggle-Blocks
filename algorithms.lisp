(in-package #:puzzle)


(defmacro seta (array index-list value)
  `(setf (aref ,array ,@index-list) ,value))

(defmacro dotimes* (var-count-list &body body)
  "Creates nested dotimes constructs: (DOTIMES* (A 3 B 4 C 5) <SOMETHING WITH A B C>)"
  (loop with new-body = `(PROGN ,@body)
        for (count var) on (reverse var-count-list) by #'cddr
        do (setf new-body `(DOTIMES (,var ,count) ,new-body))
        finally (return new-body)))

;; TODO make a nice Board iteration construct with column and row bindings.


;; Interpolations

(defun linear-interpolate (p1 p2 u)
  (+ (* (- 1.0 u) p1) (* u p2)))

(defun bounce-interpolate (p1 p2 u)
  (let ((n1 7.5625d0)
        (d1 2.75d0))
    (labels ((first-segment-p (x) (< x (/ 1.0 d1)))
             (second-segment-p (x) (< x (/ 2.0 d1)))
             (third-segment-p (x) (< x (/ 2.5 d1)))
             (bounce-segment (x offset addend)
               (+ (* n1 (- x offset)
                     (- x offset))
                  addend)))
      (let ((bounce-factor
             (cond
               ((first-segment-p u) (* n1 u u))
               ((second-segment-p u) (bounce-segment u (/ 1.5 d1) 0.75d0))
               ((third-segment-p u) (bounce-segment u (/ 2.25 d1) 0.9375d0))
               (t (bounce-segment u (/ 2.625 d1) 0.984375d0)))))
        (+ (* (- 1.0d0 bounce-factor) p1)
           (* bounce-factor p2))))))
