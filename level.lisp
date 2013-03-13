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

(defun padded (buffer)
  (with-padding 
    (units (with-difficulty 5 7 10 20 20 30 40 45 45))
    buffer))

(defun padded-vertically (a b)
  (percent-of-time 50 (rotatef a b))
  (arrange-below 
   (padded (with-border 10 a))
   (padded (with-border 10 b))))

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
  (singleton (make-wildcard)))

(defun make-two-puzzle-easy (colors)
  (destructuring-bind (A B) colors
    (horizontally
     (vertically
      (hazard)
      (horizontally 
       (bricks 6 B) 
       (hazard)))
     (bordered
      (horizontally
       (vertically
	(gated A (bricks 6 (or *required-color* B)))
	(vertically 
	 (horizontally 
	  (hazard)
	  (singleton (bulkhead)))
	 (gated B 
		(vertically (hazard)
			    (bricks 6 A)))))
       (let ((*puzzle-border* 12))
	 (either-way (wildcard)
		     (make-exit (derange (theme-colors))))))))))

(defun make-puzzle (colors)
  (cond 
    ;; with two colors, terminate the recursion
    ((= 2 (length colors))
     (make-two-puzzle-easy colors))
    ;; with three or more colors, puzzify and recurse
    ((< 2 (length colors))
     (let ((key (random-choose colors)))
       (destructuring-bind (A B C &rest other-colors) (derange colors)
	 (bordered
	  (arrange-beside
	   (padded-vertically 
	    (horizontally
	     (vertically 
	      (horizontally (gated A (bricks 6 C))
			    (hazard))
	      (horizontally
	       (vertically
		(horizontally (bricks 6 B) (hazard))
		(horizontally 
		 (gated B 
			(horizontally 
			 (hazard)
			 (bricks 5 A)))
		 (gated C
			(requiring key
			  (make-puzzle (rest colors))))))
	       (hazard)))
	     (hazard))
	    (padded-vertically 
	     (horizontally
	      (hazard)
	      (bricks 5 B))
	     (bricks 6 (or *required-color* B))))
	   (bordered 
	    (either-way 
	     (bricks 6 (random-color))
	     (either-way (bricks 6 (random-color)) (hazard)))))))))))

(defun 2x0ng-level (&optional (level 1))
  (configure-level level)
  (setf *ball* nil)
  (let ((robot (new 'player-1-robot "gold"))
	(buffer (new '2x0ng))
	(puzzle (with-border (units 8)
		  (make-puzzle (derange (level-colors))))))
    (with-buffer buffer
      (setf (%background-color (current-buffer)) (background-color))
      (bind-event buffer '(:r :control) :reset-game)
      ;;
      (paste-from buffer puzzle)
      ;; playfield border
      (wall-around-region -1 2 
			  (+ 8 (truncate (/ (%width puzzle) (units 1))))
			  (+ 8 (1- (truncate (/ (%height puzzle)
						 (units 1))))))
      ;; player 1
      (drop-object (current-buffer) robot (units 4) (units 5))
      (set-cursor (current-buffer) robot)
      (follow-with-camera (current-buffer) robot)
      ;; adjust scrolling parameters
      (setf (%window-scrolling-speed buffer) (/ *robot-speed* 2)
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 2/5)
      ;; make player temporarily invulnerable 
      (raise-shield robot)
      ;; give some instructions
      (drop-object (current-buffer) 
		   (new 'bubble (format nil "LEVEL ~S" *level*) "sans-mono-bold-22")
		   (units 8) (units 5))
      (drop-object (current-buffer) 
		   (new 'bubble "Arrow keys to move. Space (or Alt) to fire. Control-r to reset. F12 to pause." "sans-mono-bold-14")
		   (units 8) (units 7))
      ;;
      (trim (current-buffer))
;      (play-music (nth *level* *soundtrack*) :loop t)
      (current-buffer))))

