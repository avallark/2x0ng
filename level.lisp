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
    (dolist (color (level-colors))
      (fat-brick-row x y0 length color)
      (incf y0 (* *unit* *fat-brick-height*)))))

(defun super-fat-row (x y length color)
  (let ((y0 y))
    (dotimes (n 4)
      (fat-brick-row x y0 length color)
      (incf y0 (* *unit* *fat-brick-height*)))))

(defparameter *sideline-width* 2) 

;;; Music

(defresource
    (:name "remembering-xalcyon" :type :music :file "remembering-xalcyon.ogg" :properties (:volume 30))
    (:name "xioforms" :type :music :file "xioforms.ogg" :properties (:volume 30))
  (:name "xiomacs" :type :music :file "xiomacs.ogg" :properties (:volume 30))
  (:name "phong" :type :music :file "phong.ogg" :properties (:volume 20))
  (:name "xmrio" :type :music :file "xmrio.ogg" :properties (:volume 20))
  (:name "rappy" :type :music :file "rappy.ogg" :properties (:volume 30))
  (:name "invec" :type :music :file "invec.ogg" :properties (:volume 60))
  (:name "basswarp" :type :music :file "basswarp.ogg" :properties (:volume 70))
  (:name "bootypax" :type :music :file "bootypax.ogg" :properties (:volume 40))
  (:name "vrov" :type :music :file "vrov.ogg" :properties (:volume 30))
  (:name "conspiracy" :type :music :file "conspiracy.ogg" :properties (:volume 80))
  (:name "entel" :type :music :file "entel.ogg" :properties (:volume 80))
  (:name "maxmacro" :type :music :file "maxmacro.ogg" :properties (:volume 30))
  (:name "vedex" :type :music :file "vedex.ogg" :properties (:volume 50))
  (:name "rekall" :type :music :file "rekall.ogg" :properties (:volume 50))
  (:name "musicbox" :type :music :file "musicbox.ogg" :properties (:volume 50))
  (:name "saga" :type :music :file "saga.ogg" :properties (:volume 20))
  (:name "reprise" :type :music :file "reprise.ogg" :properties (:volume 12))
  (:name "ompula" :type :music :file "ompula.ogg" :properties (:volume 30)))

(defparameter *soundtrack*
  '("vedex" "remembering-xalcyon" "phong" 
    "saga" "basswarp" "entel"  
    "maxmacro" "bootypax" "musicbox" 
    "xiomacs" "xmrio" "rappy" "invec" "ompula"))

;; Wrapping things about one another

(defparameter *puzzle-border* (units 1.9))

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

(defun with-padding (amount buffer)
  (with-fields (height width) buffer
    (with-new-buffer 
      (paste-from (current-buffer) buffer amount 0) 
      (resize (current-buffer)
	      (+ width amount)
	      height))))

(defun with-automatic-padding (buffer)
  (with-border (units 3)
    (with-padding (+ (units 1) 
		     (random (units 25)))
      buffer)))

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

(defun bordered (x) 
  (assert (blockyp x))
  (with-border *puzzle-border* x))

(defun singleton (x) 
  (assert (blockyp x))
  (bordered (with-new-buffer (drop-object (current-buffer) x))))

(defun gated (color buf) (wrap (new 'gate color) buf))

(defun random-color () (random-choose (theme-colors)))

(defvar *required-color* nil)

(defmacro requiring (color &body forms)
  `(let ((*required-color* ,color)) ,@forms))

(defun hazard () 
  (singleton (make-hazard)))

(defun wildcard ()
  (singleton (or (make-wildcard)
		 (make-hazard))))

(defun stacked-up (&rest things)
  (assert things)
  (if (= 1 (length things))
      (first things)
      (arrange-below (first things) (apply #'stacked-up (rest things)))))

(defun lined-up (&rest things)
  (assert things)
  (if (= 1 (length things))
      (first things)
      (arrange-beside (first things) (apply #'lined-up (rest things)))))

(defun stacked-up-randomly (&rest things)
  (bordered (apply #'funcall #'stacked-up (derange things))))

(defun lined-up-randomly (&rest things)
  (bordered (apply #'funcall #'lined-up (derange things))))

(defun randomly (&rest things)
  (apply #'funcall (or (percent-of-time 50 #'stacked-up-randomly)
		       #'lined-up-randomly) 
	 things))

(defun laid-out (&rest things)
  (assert things)
  (apply #'funcall (if (evenp *depth*)
	     #'stacked-up
	     #'lined-up)
	 things))

(defun mixed-up (&rest things)
  (assert things)
  (apply #'funcall (if (evenp *depth*)
	     #'stacked-up-randomly
	     #'lined-up-randomly)
	 things))

(defun mixed-down (&rest things)
  (assert things)
  (apply #'funcall (if (oddp *depth*)
	     #'stacked-up-randomly
	     #'lined-up-randomly)
	 things))

(defun skewed (&rest things)
  (assert things)
  (apply #'funcall #'mixed-up (mapcar #'with-automatic-padding things)))

(defun horizontal-bulkhead (width)		   
  (let ((u (/ width 3)))
    (with-new-buffer
      (drop-object (current-buffer) (new 'wall u (units 1)) 0 0)
      (drop-object (current-buffer) (new 'wall u (units 1)) (* 2 u) 0))))

(defun vertical-bulkhead (width)		   
  (let ((u (/ width 3)))
    (with-new-buffer
      (drop-object (current-buffer) (new 'wall (units 1) u) 0 0)
      (drop-object (current-buffer) (new 'wall (units 1) u) 0 (* 2 u)))))

(defun with-bulkheads (buffer)
  (trim buffer)
  (let ((wall (if (evenp *depth*)
		  (horizontal-bulkhead (%width buffer))
		  (vertical-bulkhead (%height buffer)))))
    (laid-out (bordered wall) buffer (bordered (duplicate wall)))))

(defun with-garrisons (buffer)
  (trim buffer)
  (let* ((height (%height buffer))
	 (x (units 2))
	 (y (/ height 2))
	 (garrison (with-new-buffer 
		     (drop-object (current-buffer) 
				  (new 'base)
				  x (- y (units 4)))
		     (drop-object (current-buffer)
				   (new 'paddle)
				   x (+ y (units 18)))
		     (drop-object (current-buffer)
				   (new 'paddle)
				   x (- y (units 18)))
		     (drop-object (current-buffer) 
				  (new 'vent) x y))))
    (lined-up garrison buffer garrison)))

(defun with-outposts (buffer)
  (trim buffer)
  (let* ((height (%height buffer))
	 (x (units 2))
	 (y (/ height 2))
	 (outpost (with-new-buffer 
		     (drop-object (current-buffer) 
				  (new 'hole)
				  x (- y (units 4)))
		     (drop-object (current-buffer) 
				  (new 'hole) x y))))
    (lined-up outpost buffer outpost)))

(defun make-core (colors)
;  (assert (zerop *depth*))
  (destructuring-bind (A B) colors
    (stacked-up-randomly
      (skewed (hazard) (bricks 6 B))
     (lined-up-randomly 
      (hazard)
      (bordered
	   (stacked-up-randomly 
	    (gated A (bricks 6 (or *required-color* B)))
	    (hazard)
	    (mixed-up (hazard)
		      (bricks 6 A))))
      (let ((*puzzle-border* 12))
	(mixed-up (wildcard)
		    (make-exit (derange (theme-colors)))))))))

(defun make-layer (colors)
  (let ((*depth* (1- *depth*)))
    (cond 
      ;; with two colors, terminate the recursion
      ((= 2 (length colors))
       (make-core colors))
      ;; with three or more colors, puzzify and recurse
      ((< 2 (length colors))
       (let ((key (random-choose colors)))
	 (destructuring-bind (A B C &rest other-colors) 
	     (derange colors)
	   (stacked-up-randomly
	    (skewed (gated A (bricks 6 C))
		    (hazard)
		    (bricks 6 B))
	    (randomly
	     (stacked-up-randomly 
	      (lined-up-randomly
	       (hazard)
	       (gated B 
		      (randomly (hazard)
				(bricks 5 A)))
	       (hazard)
	       (bricks 6 (or *required-color* B)))
	      (hazard)
	      (with-bulkheads
		  (gated C
			 (requiring key
			   (funcall (if (>= *level* 9) #'with-garrisons #'identity)
				    (make-layer (rest colors))))))
	      (stacked-up-randomly (hazard) (gated (random-color) (bricks 8 C)) (hazard) (bricks 8 (random-color))))
	     (stacked-up-randomly
	      (lined-up-randomly
	       (skewed (hazard)
		       (bricks 5 A))
	       (hazard))
	      (lined-up-randomly
	       (bricks 6 (or *required-color* B))
	       (hazard)
	       (bricks 6 (random-color))))))))))))

(defun make-puzzle (colors)
  (assert (every #'stringp colors))
  (let ((*depth* (length colors))
	(puzzle (make-layer colors)))
    (case (length colors) 
      (2 puzzle)
      (3 (with-outposts puzzle))
      (4 (with-garrisons puzzle)))))

(defun 2x0ng-level (&optional (level 1))
  (configure-level level)
  (setf *ball* nil)
  (let ((robot (new 'player-1-robot "gold"))
	(buffer (new '2x0ng))
	(puzzle (with-border (units 10)
		  (make-puzzle (derange (level-colors))))))
    (with-buffer buffer
      (setf (%background-color (current-buffer)) (background-color))
      (bind-event buffer '(:r :control) :reset-game)
      ;;
      (paste-from buffer puzzle)
      ;; playfield border
      (wall-around-region -1 2 
			  (+ 10 (truncate (/ (%width puzzle) (units 1))))
			  (+ 10 (1- (truncate (/ (%height puzzle)
						 (units 1))))))
      ;; adjust scrolling parameters
      (setf (%window-scrolling-speed buffer) (/ *robot-speed* 2)
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 2/5)
      ;; give some instructions
      (drop-object (current-buffer) 
		   (new 'bubble (format nil "LEVEL ~S" *level*) "sans-mono-bold-22")
		   (units 8) (units 5))
      (drop-object (current-buffer) 
		   (new 'bubble "Arrow keys to move. Space (or Alt) to fire. Control-r to reset. F12 to pause." "sans-mono-bold-14")
		   (units 8) (units 7))
      ;;
      (trim (current-buffer))
      ;; player 1
      ;; (let ((y (or (percent-of-time 50 (units 4))
      ;; 		   (- (%height buffer) (units 7)))))
      (drop-object (current-buffer) robot (units 4) (units 5))
      (set-cursor (current-buffer) robot)
      (move-window-to-cursor (current-buffer))
      (follow-with-camera (current-buffer) robot)
      (raise-shield robot)
;      (play-music (nth *level* *soundtrack*) :loop t)
      (current-buffer))))

