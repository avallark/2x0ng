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
    (dotimes (n 3)
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
  (:name "bootypax" :type :music :file "bootypax.ogg" :properties (:volume 50))
  (:name "conspiracy" :type :music :file "conspiracy.ogg" :properties (:volume 80))
  (:name "entel" :type :music :file "entel.ogg" :properties (:volume 80))
  (:name "maxmacro" :type :music :file "maxmacro.ogg" :properties (:volume 30))
  (:name "vedex" :type :music :file "vedex.ogg" :properties (:volume 50))
  (:name "rekall" :type :music :file "rekall.ogg" :properties (:volume 50))
  (:name "musicbox" :type :music :file "musicbox.ogg" :properties (:volume 50))
  (:name "saga" :type :music :file "saga.ogg" :properties (:volume 30))
  (:name "reprise" :type :music :file "reprise.ogg" :properties (:volume 20))
  (:name "ompula" :type :music :file "ompula.ogg" :properties (:volume 30)))

(defparameter *soundtrack*
  '("vedex" "remembering-xalcyon" "phong" "saga" "basswarp" "entel" "maxmacro" "bootypax" "musicbox" "reprise"
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
	      height
	      (+ width amount)))))

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
    (units (level-value 5 7 10 20 20 30 40 45 45))
    buffer))

(defun padded-vertically (a b)
  (percent-of-time 50 (rotatef a b))
  (arrange-below 
   (padded (with-border 10 a))
   (padded (with-border 10 b))))

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
    (0 (new 'wall))
    (1 (new 'paddle))
    (2 (new 'paddle))
    (3 (new (random-choose '(paddle tracer))))
    (4 (new (random-choose '(tracer paddle))))
    (otherwise 
     (or (percent-of-time 20 (new 'robot "hot pink"))
	 (new (random-choose '(tracer paddle)))))))

(defun hazard ()
  (singleton (or (random-hazard) (new 'wall))))

(defun boss-hazard ()
  (singleton (if (> *level* 4) (new 'ghost) (random-hazard))))

;; (defun make-two-puzzle-hard (colors)
;;   (destructuring-bind (A B) colors
;;     (vertically
;;      (horizontally
;;       (singleton (new 'hole))
;;       (vertically (bricks 6 B) (hazard)))
;;      (bordered
;;       (vertically
;; 	(horizontally 
;; 	 (singleton (new 'hole))
;; 	 (gated B 
;; 		(horizontally (either-way (hazard) (hazard))
;; 			      (bricks 7 A))))
;; 	(gated A 
;; 	       (horizontally
;; 		(bricks 6 (or *required-color* B))
;; 		(let ((*puzzle-border* 12))
;; 		  (make-exit (derange (theme-colors)))))))))))

(defun make-two-puzzle-easy (colors)
  (destructuring-bind (A B) colors
    (horizontally
     (vertically
      (singleton (new 'hole))
      (horizontally (bricks 5 B) (hazard)))
     (bordered
      (horizontally
       (vertically
	(gated A (bricks 4 (or *required-color* B)))
	(vertically 
	 (singleton (new 'hole))
	 (gated B 
		(vertically (hazard)
			    (bricks 5 A)))))
       (let ((*puzzle-border* 12))
	 (either-way (boss-hazard)
		     (make-exit (derange (theme-colors))))))))))

(defun make-two-puzzle (colors) 
  (case *level*
    (0 (make-two-puzzle-easy colors))
    (1 (make-two-puzzle-easy colors))
    (2 (make-two-puzzle-hard colors))
    (3 (make-two-puzzle-easy colors))
    (4 (make-two-puzzle-hard colors))
    (5 (make-two-puzzle-hard colors))
    (6 (make-two-puzzle-easy colors))
    (7 (make-two-puzzle-hard colors))
    (8 (make-two-puzzle-hard colors))))

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
	    (padded-vertically 
	     (horizontally
	      (singleton (new 'hole))
	      (bricks 5 B))
	     (bricks 2 (or *required-color* B))))
	   (bordered (either-way (bricks 4 (random-color)) (hazard))))))))))

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
	(puzzle (with-border (units 8)
		  (make-puzzle (derange (theme-colors))))))
    (with-buffer buffer
      (paste-from buffer puzzle)
      (setf (%background-color (current-buffer)) (background-color))
      (bind-event buffer '(:r :control) :reset-game)
      ;; playfield border
      (wall-around-region -1 2 
			  (+ 16 (truncate (/ (%width puzzle) (units 1))))
			  (+ 16 (1- (truncate (/ (%width puzzle)
						 (units 1))))))
      ;; player 1
      (drop-object (current-buffer) robot (units 5) (units 5))

      ;; TEST OBJECT 
;      (drop-object (current-buffer) (new 'robot "purple") (units 15) (units 7))

      (set-cursor (current-buffer) robot)
      (follow-with-camera (current-buffer) robot)
      ;; message
      (setf (%window-scrolling-speed buffer) (/ *robot-speed* 2)
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 2/5)
      (play-sample "go.wav")
      (drop-object (current-buffer) 
		   (new 'bubble (format nil "LEVEL ~S" *level*) "sans-mono-bold-22")
		   (units 8) (units 5))
      (drop-object (current-buffer) 
		   (new 'bubble "Arrow keys to move. space to fire. control-r to reset. F12 to pause." "sans-mono-bold-14")
		   (units 8) (units 7))
      ;;
      (trim (current-buffer))
;      (play-music "bootypax" :loop t)
      (play-music (nth *level* *soundtrack*) :loop t)
      (current-buffer))))

