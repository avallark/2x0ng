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

(defparameter *fat-brick-width* 2.0)
(defparameter *fat-brick-height* 1.3)

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

(defparameter *puzzle-border* (units 1.7))

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

(defun bricks (size color)
  (with-new-buffer 
    (super-fat-row 0 0 size color)
    (trim (current-buffer))))

(defun derange (things)
  (let ((len (length things))
	(things2 (coerce things 'vector)))
    (dotimes (n len)
      (rotatef (aref things2 n)
	       (aref things2 (random len))))
    (coerce things2 'list)))

(defun make-exit (colors)
  (cond
    ((null colors)
     (with-new-buffer (add-object (current-buffer) (new 'exit))))
    ((consp colors)
     (wrap (new 'gate (first colors))
	   (make-exit (rest colors))))))

(defun horizontally (a b)
  (percent-of-time 50 (rotatef a b))
  (arrange-beside 
   (with-border 10 a)
   (with-border 10 b)))

(defun vertically (a b)
  (percent-of-time 50 (rotatef a b))
  (arrange-below 
   (with-border 10 a)
   (with-border 10 b)))

(defun either-way (a b)
  (funcall (or (percent-of-time 50 #'horizontally) #'vertically)
	   a b))

(defun bordered (x) (with-border *puzzle-border* x))

(defun singleton (x) (bordered (with-new-buffer (drop-object (current-buffer) x))))

(defun gated (color buf) (wrap (new 'gate color) buf))

(defun random-color () (random-choose (theme-colors)))

(defvar *required-color* nil)

(defmacro requiring (color &body forms)
  `(let ((*required-color* ,color)) ,@forms))

(defun random-hazard ()
  (case *level*
    (0 nil)
    (1 (new 'monitor))
    (2 (new 'tracer))
    (3 (new 'tracer))
    (otherwise (new (random-choose '(paddle tracer))))))

(defun hazard ()
  (singleton (or (random-hazard) (new 'wall))))

(defun make-puzzle (colors)
  (cond 
    ;; with two colors, terminate the recursion
    ((= 2 (length colors))
     (destructuring-bind (A B) colors
       (vertically
	(vertically
	 (singleton (new 'hole))
	 (bricks 4 B))
	(bordered
	 (horizontally
	  (vertically
	   (gated A (bricks 3 (or *required-color* B)))
	   (vertically 
	    (singleton (new 'hole))
	    (gated B 
		   (bricks 4 A))))
	  (let ((*puzzle-border* 12))
	    (make-exit (derange (theme-colors)))))))))
    ;; with three or more colors, puzzify and recurse
    ((< 2 (length colors))
     (let ((key (random-choose colors)))
       (destructuring-bind (A B C &rest other-colors) (derange colors)
	 (bordered
	  (horizontally 
	   (horizontally
	    (vertically 
	     (horizontally (gated A (bricks 4 C))
			   (singleton (new 'hole)))
	     (horizontally
	      (vertically
	       (horizontally (bricks 4 B) (bricks 3 (random-choose (theme-colors))))
	       (horizontally 
		(gated B 
		       (horizontally 
			(singleton (new 'hole))
			(bricks 2 A)))
		(gated C
		       (requiring key
			 (make-puzzle (rest colors))))))
	      (hazard)))
	    (hazard))
	   (vertically 
	    (horizontally
	     (singleton (new 'hole))
	     (bricks 5 B))
	    (bricks 2 (or *required-color* B))))))))))

(defparameter *level-themes* 
  '(:xalcrys :snefru :zupro :krez :snafu :atlantis :zerk :tandy :command))

(defun level-theme (n)
  (first (nth (mod n (length *level-themes*)) *level-themes*)))

(defun make-theme-sequence ()
  (append
   (derange *two-brick-themes*)
   (derange *three-brick-themes*)
   (derange *four-brick-themes*)))

(defun 2x0ng-level (&optional (level 0))
  (setf *level* level)
  (set-theme (level-theme *level*))
  (setf *ball* nil)
  (let ((robot (new 'player-1-robot "gold"))
	(buffer (new '2x0ng))
	(puzzle (with-border (units 20)
		  (make-puzzle (derange (theme-colors))))))
    (with-buffer buffer
      (paste-from buffer puzzle)
      (setf (%background-color (current-buffer)) (background-color))
      (bind-event buffer '(:r :control) :reset-game)
      ;; playfield border
      (wall-around-region -1 2 
			  (+ 40 (truncate (/ (%width puzzle) (units 1))))
			  (+ 40 (1- (truncate (/ (%width puzzle)
						 (units 1))))))
      ;; player 1
      (drop-object (current-buffer) robot (units 5) (units 5))
      (set-cursor (current-buffer) robot)
      (follow-with-camera (current-buffer) robot)
      ;; message
      (setf (%window-scrolling-speed buffer) 5
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 2/5)
      (play-sample "go.wav")
      (drop-object (current-buffer) 
		   (new 'bubble (format nil "LEVEL ~S GO!" *level*))
		   (units 8) (units 6))
      ;;
      (trim (current-buffer))
      (current-buffer))))

