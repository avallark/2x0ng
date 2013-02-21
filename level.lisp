(in-package :2x0ng)

;; Making walls

(defun wall-at (x y width height &optional (thing 'wall))
  (labels ((unit (&rest summands)
	     (* *unit* (reduce #'+ summands :initial-value 0))))
    (let ((wall (new thing (wall-color))))
      (drop-object (current-buffer) wall (unit x) (unit y))
      (resize wall (unit width) (unit height)))))

(defun wall-around-region (x y width height)
    (let ((left x)
	  (top y)
	  (right (+ x width))
	  (bottom (+ y height)))
      ;; top wall
      (wall-at left top (- right left) 1)
      ;; bottom wall
      (wall-at left bottom (- right left -1) 1)
      ;; left wall
      (wall-at left top 1 (- bottom top))
      ;; right wall
      (wall-at right top 1 (- bottom top -1))))

;;; Making rows of breakable bricks

(defparameter *fat-brick-width* 3)
(defparameter *fat-brick-height* 2)

(defun make-fat-brick (&optional color)
  (let ((brick (new 'brick color)))
    (prog1 brick
      (resize brick (* *unit* *fat-brick-width*) (* *unit* *fat-brick-height*)))))

(defun fat-brick-row (x y length color)
  (dotimes (n length)
    (let ((brick (make-fat-brick color)))
      (drop-object (current-buffer) brick (+ x (* n *unit* *fat-brick-width*)) y))))

(defun themed-row (x y length)
  (let ((y0 y))
    (dolist (color (brick-colors))
      (fat-brick-row x y0 length color)
      (incf y0 (* *unit* *fat-brick-height*)))))

(defun super-fat-row (x y length color)
  (let ((y0 y))
    (dotimes (n 3)
      (fat-brick-row x y0 length color)
      (incf y0 (* *unit* *fat-brick-height*)))))

(defparameter *sideline-width* 2) 

(defun 2x0ng-level 
    (&key
       (difficulty 0)
       (width *level-width*)
       (height *level-height*))
  (setf *ball* nil)
  ;; (setf *theme* '("black" "gray20" "dark orange" "orange" "gold"))
  ;; (setf *theme* '("red" "orange" "yellow" "blue" "green" "purple"))
  (let ((buffer (new '2x0ng))
	(robot (new 'player-1-robot "gold")))
    (set-random-theme)
    (setf (%background-color buffer) (background-color))
    (prog1 buffer
      (with-buffer buffer
	(bind-event buffer '(:r :control) :reset)
	;; playfield border
	(wall-around-region -1 2 width (- height 1))
	;; player 1
	(drop-object buffer robot (units 5) (units 5))
	(set-cursor buffer robot)
	(follow-with-camera (current-buffer) robot)
	;; rows of multicolored bricks
	;; (themed-row (units 4) (units 15) 13)
	;; (themed-row (units 8) (units 35) 5)
	;; (themed-row (units 4) (units 50) 13)
	
	(dotimes (n 15)
	  (super-fat-row (+ (units 10) (units (random 60)))
			 (+ (units 10) (units (random 60)))
			 (+ 3 (random 7))
			 (random-choose (brick-colors))))

	;;
	(drop-object buffer robot (units 10) (units 6))
	;;
	(trim buffer)))))

	  ;; (%window-scrolling-speed buffer) 4.5
	  ;; (%horizontal-scrolling-margin buffer) 2/5
	  ;; (%vertical-scrolling-margin buffer) 2/5)
