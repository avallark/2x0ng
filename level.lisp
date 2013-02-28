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
    (dotimes (n 2)
      (fat-brick-row x y0 length color)
      (incf y0 (* *unit* *fat-brick-height*)))))

(defparameter *sideline-width* 2) 

;; Wrapping things about one another

(defparameter *puzzle-border* (units 1.4))

(defun wrap (thing buffer)
  (multiple-value-bind (top left right bottom)
      (find-bounding-box (get-objects buffer))
    (prog1 buffer
      (with-buffer buffer
	(add-object buffer thing 
		    (- left *puzzle-border*)
		    (- top *puzzle-border*))
	(resize thing 
		(+ (* 2 *puzzle-border*) (- right left))
		(+ (* 2 *puzzle-border*) (- bottom top)))
	(trim (current-buffer))))))

(defparameter *color-phase* 0)

(defun nth-color (n)
  (nth (mod (+ n *color-phase*) (length (theme-colors)))
       (theme-colors)))

(defun fat-buffer (size color)
  (with-new-buffer 
    (super-fat-row 0 0 size color)
    (trim (current-buffer))))

(defun make-puzzle (colors)
  (cond
    ((null colors)
     (with-new-buffer (add-object (current-buffer) (new 'exit))))
    ((consp colors)
     (wrap (new 'gate (first colors))
	   (with-border *puzzle-border* (make-puzzle (rest colors)))))))

(defun make-level (depth)
  (cond ((zerop depth)
	 (fat-buffer depth (nth-color depth)))
	((plusp depth)
	 (with-border 10
	   (arrange-below 
	    (arrange-beside 
	     (wrap (new 'gate (nth-color (- depth 1)))
		   (fat-buffer depth (nth-color (- depth 4))))
	     (wrap (new 'gate (nth-color (- depth 2)))
		   (fat-buffer depth (nth-color (+ depth 1)))))
	    (arrange-beside (make-level (- depth 1))
			   (wrap (new 'gate (nth-color depth))
				 (with-border *puzzle-border* 
				   (arrange-below
				    (fat-buffer depth (nth-color (- depth 1)))
				    (fat-buffer depth (nth-color (+ depth 1))))))))))))

(defun derange (things)
  (let ((len (length things))
	(things2 (coerce things 'vector)))
    (dotimes (n len)
      (rotatef (aref things2 n)
	       (aref things2 (random len))))
    (coerce things2 'list)))

(defun 2x0ng-level 
    (&key
       (difficulty 0)
       (width *level-width*)
       (height *level-height*))
  (setf *ball* nil)
  (let ((buffer (new '2x0ng))
	(robot (new 'player-1-robot "gold")))
    (set-theme :zerk)
    (setf (%background-color buffer) (background-color))
    (prog1 buffer
      (with-buffer buffer
	(bind-event buffer '(:r :control) :reset)
	;; playfield border
	(setf *color-phase* (random 4))
	(wall-around-region -1 2 width (- height 1))
	;; player 1
	(drop-object buffer robot (units 5) (units 5))
	(set-cursor buffer robot)
	(follow-with-camera (current-buffer) robot)
	;; 
	(paste-from (current-buffer) 
		    (arrange-below
		     (make-level 1)
		     (arrange-beside
		      (make-level 2) (make-puzzle (derange (theme-colors)))))
		    (units 10) (units 12))
	
	;;
	(trim buffer)))))

	  ;; (%window-scrolling-speed buffer) 4.5
	  ;; (%horizontal-scrolling-margin buffer) 2/5
	  ;; (%vertical-scrolling-margin buffer) 2/5)
