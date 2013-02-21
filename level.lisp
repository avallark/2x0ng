(in-package :2x0ng)

(defparameter *themes* 
  '((:dec "DarkSlateBlue" "SlateBlue" "magenta" "yellow")
    (:vcs "black" "red" "pink" "orange")
    (:tandy "DarkBlue" "red" "yellow" "orange")
    (:vax "gray12" "orange" "goldenrod" "cyan")
    (:command "black" "DarkGoldenrod" "red" "blue")
    (:surround "DarkOliveGreen" "GreenYellow" "yellow" "tomato")
    (:maynard "saddle brown" "DarkOrange" "yellow" "DeepSkyBlue")
    (:zerk "black" "maroon2" "cyan" "salmon")
    (:wizard "midnight blue" "slate blue" "dark orange" "orange" "gold")))

(defun find-theme (name)
  (assoc name *themes*))

(defparameter *theme* (find-theme :wizard))

(defun theme-colors (&optional (theme *theme*))
  (rest (rest theme)))

(defun random-theme () (random-choose (mapcar #'car *themes*)))

(defun background-color ()
  (when *theme* (first (theme-colors))))

(defun wall-color ()
  (when *theme* (second (theme-colors))))

(defun brick-colors ()
  (when *theme* (rest (rest (theme-colors)))))

;; Making walls

(defun wall-at (x y width height &optional (thing 'wall))
  (labels ((unit (&rest summands)
	     (* *unit* (reduce #'+ summands :initial-value 0))))
    (let ((wall (new thing)))
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

(defparameter *sideline-width* 2) 

(defun 2x0ng-level 
    (&key
       (difficulty 0)
       (width *level-width*)
       (height *level-height*))
  (setf *ball* nil)
  (setf *theme* '("black" "gray20" "dark orange" "orange" "gold"))
  (let ((buffer (new '2x0ng))
	(robot (new 'player-1-robot "gold")))
    (setf (%background-color buffer) "gray20")
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
	(themed-row (units 4) (units 15) 13)
	(themed-row (units 8) (units 35) 5)
	(themed-row (units 4) (units 50) 13)
	;;
	(drop-object buffer robot (units 10) (units 6))
	;;
	(trim buffer)))))

	  ;; (%window-scrolling-speed buffer) 4.5
	  ;; (%horizontal-scrolling-margin buffer) 2/5
	  ;; (%vertical-scrolling-margin buffer) 2/5)
