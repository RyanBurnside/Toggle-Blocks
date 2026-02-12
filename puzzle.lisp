;;;; puzzle.lisp

(in-package #:puzzle)
(export 'main)

(defparameter *sprites* nil)

(defparameter *colors*
  (loop for i from 0 below 5 collect i))

(defparameter *font* nil)

(defun draw-block (x y block &key (show-color2 t) (show-inverted nil))
  (let* ((new-x (+ x (* (x block) 32)))
	 (new-y (+ y (* (y block) 32)))
	 (col1 (if show-inverted
		   (color2 block)
		   (color1 block)))
	 (col2 (if show-inverted
		   (color1 block)
		   (color2 block)))
	 (offset1 (* col1 32)) ; Subimage offsets
	 (offset2 (* col2 32)))
    (al:draw-bitmap-region *sprites* offset1 0 32 32 new-x new-y nil)
    (when show-color2
	(al:draw-bitmap-region *sprites* offset2 32 32 32 new-x new-y nil))))

(defun draw-piece (piece &key (reversed nil))
  (dolist (b (blocks piece))
       (draw-block b :reversed reversed)))

(defun draw-board (x y board)
  (al:draw-filled-rectangle x
			    y
			    (+ x (1- (* (width board) 32)))
			    (+ y (1- (* (height board) 32)))
			    (al:map-rgb 64 0 128))

  (dotimes* (yy (height board)
	     xx (width board))
    (when (aref (blocks board) yy xx)
      (draw-block x y (aref (blocks board) yy xx)
		  :show-color2 nil))))
	
(defun make-dummy-board (rows columns)
  (let ((dummy (make-board rows columns)))
    (dotimes* (c columns r rows)
      (setf (aref (blocks dummy) r c)
	    (make-random-block c r *colors*)))
    dummy))

(defparameter *boards*
  (loop repeat 4
	collect (make-dummy-board 12 5)))

(defun draw-boards (x y list)
  (loop with origin = x
	for b in *boards*
	for adjusted-width = (* (1+ (width b)) 32)
	do (draw-board origin y b)
	   (incf origin adjusted-width)))


;; Creates a 800x600 resizable OpenGL display titled "Simple"
;; Fixed timestep loop runs logic at 1 FPS
;; The remaining time is spent on render
;;
;; The PREVIOUS-KEY slot is user-defined state for this example
(defclass window (al:system)
  ((previous-key :initform "Nothing" :accessor previous-key))
  (:default-initargs
   :title "Toggle Blocks"
   :width 800 :height 600
   :logic-fps 30
   :display-flags '(:windowed :opengl :resizable)
   :display-options '((:sample-buffers 1 :suggest)
                      (:samples 4 :suggest)))) ; Todo look this stuff up

;; This method will be invoked after the default
;; `al:initialize-system' method
(defmethod al:initialize-system :after (system)
  (al:init-primitives-addon)
  (al:init-image-addon)
  (al:init-font-addon)
  (setf *sprites* (al:load-bitmap "./sprites.png")))

(defmethod al:update ((sys window))
  (+ 1 1))

(defmethod al:render ((sys window))
  (al:clear-to-color (al:map-rgb 0 0 0))
  (draw-boards 32 96 *boards*)
  (al:flip-display))

;; The lisp interface runs handlers during the logic step
;; Handlers are defined according to allegro events

(defmethod al:key-down-handler ((sys window))
  (let ((keyboard (cffi:mem-ref (al:event sys) '(:struct al:keyboard-event))))
    (print (getf keyboard 'al::keycode))

    ;; Dummy function for visual testing of group removal process
    (loop for done = t
	  do (dolist (b *boards*)
	       (delete-groups b 3)
	       (when (compress-blocks-down b)
		 (setf done nil)))
	  until done)
    (setf (previous-key sys) (getf keyboard 'al::keycode))))

(defun main ()
  (let ((w (make-instance 'window)))
    (al:run-system  w)
    (al:destroy-display w)               
    (al:uninstall-system)))

