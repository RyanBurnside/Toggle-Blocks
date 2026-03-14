;;;; puzzle.lisp

(in-package #:puzzle)
(export 'main)

(defparameter *sprites* nil)
(defparameter *tile-size* 32)
(defparameter *colors* (list 0 1 2 3 4))

(defparameter *font* nil)

(defun make-dummy-board (rows columns)
  (let ((dummy (make-board rows columns)))
    (dotimes* (c columns r rows)
      (set-at dummy r c (make-random-block c r *colors*)))
    dummy))

(defparameter *boards*
  (loop repeat 4
        collect (make-dummy-board 12 5)))

;; this is for the sprite itself, ignores piece-block position
(defun draw-block-sprite (x y color1 color2 &key (show-color2) (show-inverted nil))
  "Draws a block sprite at a given screen position."
  (let* ((col1 (if show-inverted color2 color1))
         (col2 (if show-inverted color1 color2))
         (offset1 (* col1 32)) ; Subimage offsets
         (offset2 (* col2 32)))
    (al:draw-bitmap-region *sprites* offset1 0 *tile-size* *tile-size* x y nil)
    (when show-color2
      (al:draw-bitmap-region *sprites* offset2 *tile-size* *tile-size* *tile-size* x y nil))))

(defun draw-block (x y block &key (show-color2 t) (show-inverted nil))
  "Assumes drawing a block from inside a board array, does position scailing."
  (let* ((new-x (+ x (* (x block) *tile-size*)))
         (new-y (+ y (* (y block) *tile-size*))))
    (draw-block-sprite new-x
                       new-y
                       (color1 block)
                       (color2 block)
                       :show-color2 show-color2
                       :show-inverted show-inverted)
    (when (marked block)
      (al:draw-rounded-rectangle (+ new-x 2) (+ new-y 2)
                                 (+ new-x 30) (+ new-y 30)
                                 8 8
                                 (al:map-rgb 255 255 255)
                                 2))))

(defun draw-piece (piece &key (reversed nil))
  (dolist (b (blocks piece))
    (draw-block b :reversed reversed)))

(defun draw-block-animation (board ba)
  (destructuring-bind (x y) (current-position ba)
    (draw-block-sprite x
                       y
                       (color1 (piece-block ba))
                       (color2 (piece-block ba))
                       :show-color2 t)))

(defun draw-board (x y board)
  (al:draw-filled-rectangle x
                            y
                            (+ x (1- (* (width board) *tile-size*)))
                            (+ y (1- (* (height board) *tile-size*)))
                            (al:map-rgb 64 0 128))
  (do-board (yy xx board)
    (when (piece-at board yy xx)
      (draw-block x y (piece-at board yy xx)
                  :show-color2 nil))))

(defun draw-boards (x y list)
  (loop with origin = x
        for b in list
        for adjusted-width = (* (1+ (width b)) *tile-size*)
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
   :logic-fps 60
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
  ;; TODO this might be slow to do per frame
  (dolist (b *boards*)
    (isolate-groups b 3)))

(defmethod al:render ((sys window))
  (al:clear-to-color (al:map-rgb 0 0 0))
  (draw-boards 32 96 *boards*)
  (al:flip-display))

;; The lisp interface runs handlers during the logic step
;; Handlers are defined according to allegro events

(defmethod al:key-down-handler ((sys window))
  (let ((keyboard (cffi:mem-ref (al:event sys) '(:struct al:keyboard-event))))
    (print (getf keyboard 'al::keycode))

    (dolist (b *boards*)
      (case (getf keyboard 'al::keycode)
        (:enter
         (format *standard-output* "Board ~a: Deleted ~a groups!~%" b (length (isolate-groups b 3)))
         (delete-groups b 3)
         (compress-blocks-down b))))

    ;; Dummy function for visual testing of group removal process
    ;; (loop for done = t
    ;;       do (dolist (b *boards*)
    ;;            (delete-groups b 3)
    ;;            (when (compress-blocks-down b)
    ;;              (setf done nil)))
    ;;       until done)


    (setf (previous-key sys) (getf keyboard 'al::keycode))))



;; 1) new piece generates
;; 2) piece falling, accepting user input.
;; 3) piece stop in location.
;; 4) patch discovery and removal.
;; 5) pieces fall in animation until can't move anymore.
;; 6) goto 3 until no new pieces are falling.
;; 7) goto 1.

(defun main ()
  (let ((w (make-instance 'window)))
    (al:run-system w)
    (al:destroy-display w)
    (al:uninstall-system)))
