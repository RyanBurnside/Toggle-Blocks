(in-package #:puzzle)

(defmacro dotimes* (var-count-list &body body)
  "Creates nested dotimes constructs: (DOTIMES* (A 3 B 4 C 5) <SOMETHING WITH A B C>)"
  (loop with new-body = `(PROGN ,@body)
        for (count var) on (reverse var-count-list) by #'cddr
        do (setf new-body `(DOTIMES (,var ,count) ,new-body))
        finally (return new-body)))

;; TODO make a nice Board iteration construct with column and row bindings.
