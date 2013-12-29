(in-package :2x0ng)

;;; "Now loading..." screen

(defresource "loading.png")

(define-buffer loading-screen
  (width :initform 1280)
  (height :initform 720)
  (level :initform 1)
  (clock :initform 20)
  (quadtree-depth :initform 4)
  (background-image :initform "loading.png"))

(defun loading-screen (&optional (level 1))
  (let ((screen (new 'loading-screen)))
    (setf (field-value :level screen) level)
    (find-object screen)))

(define-method update loading-screen ()
  (decf %clock)
  (when (zerop %clock)
    (switch-to-buffer (2x0ng-level %level))))

(defun begin-game (level)   
  ;; either win game or go to next level
  (let ((old-buffer (current-buffer)))
    (if (> level 16)
	(show-ending)
	(progn 
	  (switch-to-buffer (loading-screen level))
	  (at-next-update (destroy old-buffer))
	  (play-sample "newball.wav")))))
  
(defun reset-level ()
  (begin-game *level*))

(defun next-level ()
  (begin-game (1+ *level*)))
  
;;; Title screen

(defresource "title-sbcl.png")
(defresource "title-ccl.png")
(defresource "title-jp.png")

(defun title-screen-image () 
  #+sbcl "title-sbcl.png"
  #+ccl "title-ccl.png")

(define-buffer title 
  (quadtree-depth :initform 4)
  (background-image :initform (title-screen-image)))

(define-method start-playing title ()
  (sleep 0.2) ;; allow time for human to remove finger from spacebar
  (begin-game *level*))

;;; Ending screen

(defresource "ending.png")

(defparameter *ending-scroll-speed* 0.4)

(define-buffer ending-screen
  (quadtree-depth :initform 4)
  (width :initform 1280)
  (height :initform 720)
  (background-color :initform "black"))

(defblock scroll :image "ending.png")

(define-method update scroll ()
  (when (plusp %y)
    (move-toward self :up *ending-scroll-speed*)))

(defun show-ending ()
  (switch-to-buffer (new 'ending-screen))
  (let ((scroll (new 'scroll)))
    (insert scroll 0 720)
    (resize scroll 1280 720))
  (play-music "theme.ogg" :loop t))

;;; Help screen

(defresource "help.png")

(define-buffer help-screen
  (quadtree-depth :initform 3)
  (game :initform nil)
  (background-image :initform "help.png"))

(define-method resume-playing help-screen ()
  (sleep 0.2) ;; allow time for human to remove finger from spacebar
  (switch-to-buffer %game))

(defun help-buffer ()
  (let ((buffer (find-object (new 'help-screen))))
    (setf (field-value :game buffer) (current-buffer))
    (bind-event buffer '(:h :control) :resume-playing)
    (bind-event buffer '(:escape) :resume-playing)
    (bind-event buffer '(:space) :resume-playing)
    buffer))

;; Greeting

(defresource "greeting.png")

(defblock greeting :image "greeting.png" :collision-type nil)

(defmethod initialize :after ((self xblock) &key)
  (resize-to-image self))

;; Making walls

(defun wall-at (x y width height &optional (thing 'wall))
  (labels ((unit (&rest summands)
	     (* *unit* (reduce #'+ summands :initial-value 0))))
    (let ((wall (new thing :color (wall-color))))
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
  (let ((brick (new 'brick :color color)))
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

(defun themed-row-buffer (length)
  (with-new-buffer
    (themed-row 0 0 length)
    (trim (current-buffer))))

(defun super-fat-row (x y width height color)
  (let ((y0 y))
    (dotimes (n height)
      (fat-brick-row x y0 width color)
      (incf y0 (* *unit* *fat-brick-height*)))))

(defparameter *sideline-width* 2) 

;;; Music

(defresource
    (:name "remembering-xalcyon.ogg" :type :music :properties (:volume 30))
    (:name "xioforms.ogg" :type :music :properties (:volume 60))
  (:name "xiomacs.ogg" :type :music :properties (:volume 30))
  (:name "phong.ogg" :type :music :properties (:volume 10))
  (:name "flyby.ogg" :type :music :properties (:volume 30))
  (:name "sparqq.ogg" :type :music :properties (:volume 30))
  (:name "xmrio.ogg" :type :music :properties (:volume 20))
  (:name "rappy.ogg" :type :music :properties (:volume 20))
  (:name "invec.ogg" :type :music :properties (:volume 60))
  (:name "basswarp.ogg" :type :music :properties (:volume 70))
  (:name "bootypax.ogg" :type :music :properties (:volume 60))
  (:name "vrov.ogg" :type :music  :properties (:volume 24))
  (:name "conspiracy.ogg" :type :music :properties (:volume 60))
  (:name "entel.ogg" :type :music  :properties (:volume 80))
  (:name "maxmacro.ogg" :type :music  :properties (:volume 30))
  (:name "vedex.ogg" :type :music  :properties (:volume 50))
  (:name "rekall.ogg" :type :music  :properties (:volume 80))
  (:name "musicbox.ogg" :type :music  :properties (:volume 50))
  (:name "saga.ogg" :type :music  :properties (:volume 20))
  (:name "nexttime.ogg" :type :music  :properties (:volume 80))
  (:name "beatup.ogg" :type :music  :properties (:volume 40))
  (:name "frantix.ogg" :type :music :properties (:volume 12))
  (:name "metro.ogg" :type :music  :properties (:volume 12))
  (:name "theme.ogg" :type :music  :properties (:volume 12))
  (:name "reprise.ogg" :type :music :properties (:volume 7))
  (:name "ompula.ogg" :type :music  :properties (:volume 30)))

(defparameter *soundtrack* '("vedex.ogg" "phong.ogg"
    "saga.ogg" "basswarp.ogg" "entel.ogg" "reprise.ogg" "flyby.ogg" "sparqq.ogg" "vrov.ogg"
    "maxmacro.ogg" "bootypax.ogg" "musicbox.ogg" "frantix.ogg" "conspiracy.ogg" "phong.ogg"
    "xiomacs.ogg" "xmrio.ogg" "rappy.ogg" "invec.ogg" "ompula.ogg"))

(defparameter *boss-music* "xioforms.ogg")

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

(defun bricks (color &optional (size 6))
  (bordered
   (with-new-buffer
     (super-fat-row 0 0 
		    (+ 3 (random 5))
		    (- 6 *depth*)
		    color)
     (trim (current-buffer)))))

(defun derange (things)
  (let ((len (length things))
	(things2 (coerce things 'vector)))
    (dotimes (n len)
      (rotatef (aref things2 n)
	       (aref things2 (random len))))
    (coerce things2 'list)))

(defvar *exit* nil)

(defun make-exit (colors)
  (let ((*puzzle-border* 18))
    (cond
      ((null colors)
       (with-new-buffer (add-object (current-buffer) 
				    (setf *exit* (new 'exit)))))
      ((consp colors)
       (wrap (new 'gate :color (first colors))
	     (make-exit (rest colors)))))))

(defvar *extra-padding* 0)
  
(defun with-hpadding (amount buffer)
  (with-field-values (height width) buffer
    (with-new-buffer 
      (paste-into (current-buffer) buffer amount 0) 
      (resize (current-buffer)
	      height
	      (+ width amount *extra-padding*)))))


(defun with-vpadding (amount buffer)
  (with-field-values (height width) buffer
    (with-new-buffer 
      (paste-into (current-buffer) buffer 0 amount) 
      (resize (current-buffer)
	      (+ height amount *extra-padding*)
	      width))))

(defun with-automatic-padding (buffer)
  (with-border (units 2)
    (let ((padding (+ (units 1) 
		      (random (units 25)))))
      (if (evenp *depth*)
	  (with-hpadding padding buffer)
	  (with-vpadding padding buffer)))))

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
  (assert (xelfp x))
  (with-border *puzzle-border* x))

(defun singleton (x) 
  (assert (xelfp x))
  (bordered (with-new-buffer (drop-object (current-buffer) (find-object x)) (trim (current-buffer)))))

(defun gated (color buf) (wrap (new 'gate :color color) buf))

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
  (let ((*depth* (1+ *depth*)))
    (assert things)
    (apply #'funcall (if (evenp *depth*)
			 #'stacked-up
			 #'lined-up)
	   things)))
  
(defun mixed-up (&rest things)
  (assert things)
  (let ((layout (if (evenp *depth*)
		    #'stacked-up-randomly
		    #'lined-up-randomly)))
    (let ((*depth* (1+ *depth*)))
      (apply #'funcall layout things))))

(defun mixed-down (&rest things)
  (assert things)
  (let ((layout (if (oddp *depth*)
		    #'stacked-up-randomly
		    #'lined-up-randomly)))
    (let ((*depth* (1+ *depth*)))
      (apply #'funcall layout things))))

(defun skewed (&rest things)
  (assert things)
  (apply #'funcall #'mixed-up (mapcar #'with-automatic-padding things)))

(defun horizontal-bulkhead (width)		   
  (let ((u (/ width 5)))
    (with-new-buffer
      (drop-object (current-buffer) (new 'wall :width (* 2 u) :height (units 1)) 0 0)
      (drop-object (current-buffer) (new 'wall :width (* 2 u) :height (units 1)) (* 3 u) 0))))

(defun vertical-bulkhead (width)		   
  (let ((u (/ width 5)))
    (with-new-buffer
      (drop-object (current-buffer) (new 'wall :width (units 1) :height (* 2 u)) 0 0)
      (drop-object (current-buffer) (new 'wall :width (units 1) :height (* 2 u)) 0 (* 3 u)))))

(defun make-bulkhead-buffer (horizontal size)
  (if horizontal
      (horizontal-bulkhead size)
      (vertical-bulkhead size)))

(defun with-bulkheads (buffer &optional (horizontal (percent-of-time 50 t)))
  (trim buffer)
  (if horizontal
      (stacked-up (bordered (make-bulkhead-buffer t (%width buffer)))
		  (bordered buffer) 
		  (bordered (make-bulkhead-buffer t (%width buffer))))
      (lined-up (bordered (make-bulkhead-buffer nil (%height buffer)))
		(bordered buffer)
		(bordered (make-bulkhead-buffer nil (%height buffer))))))

(defun garrison (x y)
  (with-new-buffer 
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
		 (new 'vent) x y)
    (current-buffer)))

(defun with-garrisons (buffer)
  (trim buffer)
  (let* ((height (%height buffer))
	 (x (units 2))
	 (y (/ height 2)))
    (lined-up 
     (stacked-up (garrison x y) (bricks (random-color) 3))
     buffer
     (stacked-up (garrison x y) (bricks (random-color) 3)))))

(defun with-outposts (buffer)
  (trim buffer)
  (let* ((height (%height buffer))
	 (x (units 2))
	 (y (/ height 2))
	 (outpost (with-new-buffer 
		     (drop-object (current-buffer) 
				  (new 'paddle)
				  x (- y (units 6)))
		     (drop-object (current-buffer) 
				  (new 'hole)
				  x (- y (units 4)))
		    (when (> *difficulty* 4)
		      (drop-object (current-buffer) 
				   (new 'hole) x y)))))
    (lined-up outpost buffer outpost)))

(defun with-fortification (buffer)
  (cond 
    ((>= *level* 7) (with-garrisons buffer))
    ((>= *level* 3) (with-outposts buffer))
    (t buffer)))

(defun make-puzzle-2 (colors)
  (destructuring-bind (A B) colors
    (mixed-up
     (skewed (hazard) (bricks B))
     (hazard)
     (bordered
      (mixed-down 
       (gated A (bricks (or *required-color* B)))
       (hazard)
       (mixed-up (hazard)
		 (bricks A))))
     (let ((*puzzle-border* 14))
       (mixed-down (wildcard)
		   (make-exit (derange (theme-colors))))))))

(defun puzzle-3-components (colors)
  (assert (= 3 (length colors)))
  (let ((key (random-choose colors)))
    (destructuring-bind (A B C) (derange colors)
      (list
       ;;
       (mixed-down
	(gated A (bricks C))
	(hazard)
	(bricks B)
	(mixed-up
	 (mixed-up 
	  (hazard)
	  (gated B 
		 (randomly (hazard)
			   (bricks A)))
	  (hazard)
	  (bricks (or *required-color* B)))
	  (hazard)))
       ;;
       (mixed-down
	(gated B
	       (randomly
		(hazard) 
		(gated (random-color) 
		       (bricks C)) 
		(hazard) 
		(bricks A)))
	(with-fortification
	    (with-bulkheads
		(gated C
		       (requiring key (make-puzzle-2 (rest colors)))))))
       ;;
       (randomly
	(skewed (hazard)
		(bricks A)
		(hazard))
	(mixed-up
	 (bricks (or *required-color* C))
	 (hazard)
	 (bricks (random-color))))))))

(defun make-puzzle-3 (colors)
  (apply #'mixed-up
	 (puzzle-3-components colors)))

(defun make-puzzle-4b (colors)
  (assert (= 4 (length colors)))
  (let ((key (random-choose colors)))
    (destructuring-bind (A B C D) 
	(derange colors)
      (mixed-up
       ;;
       (mixed-down
	(gated A (bricks C))
	(hazard)
	(bricks B)
	(gated D
	       (mixed-up
		(mixed-up 
		 (hazard)
		 (gated B 
			(randomly (hazard)
				  (bricks A)))
		 (hazard)
		 (bricks C))))
	 (bricks D)
	 (hazard)))
       ;;
       (mixed-down
	(gated D
	       (randomly
		(hazard) 
		(gated (random-color) 
		       (bricks B)) 
		(hazard) 
		(bricks A)))
	(with-fortification
	    (with-bulkheads
		(gated C
		       (mixed-up
			(skewed (hazard) (bricks B))
			(hazard)
			(bordered
			 (mixed-down 
			  (gated A (bricks (or *required-color* B)))
			  (hazard)
			  (mixed-up (hazard)
				    (bricks A))))
			(let ((*puzzle-border* 14))
			  (mixed-down (wildcard)
				      (make-exit (derange (theme-colors)))))))))
	;;
	(gated B (randomly
		  (skewed (hazard)
			  (bricks A)
			  (hazard))
		  (mixed-up
		   (bricks C)
		   (bricks D)
		   (hazard)
		   (bricks (random-color)))))))))

;; (defun make-puzzle-4b (colors)
;;   (assert (= 4 (length colors)))
;;   (let ((key (random-choose colors)))
;;     (destructuring-bind (A B C D) 
;; 	(derange colors)
;;       (mixed-up
;;        ;;
;;        (mixed-down
;; 	(gated A (bricks D))
;; 	(hazard)
;; 	(bricks A)
;; 	(gated D
;; 	       (mixed-up 
;; 		(hazard)
;; 		(gated B 
;; 		       (randomly (hazard)
;; 				 (bricks C)))
;; 		(hazard)
;; 		(bricks B)))
;; 	(bricks D)
;; 	(hazard))
;;        ;;
;;        (mixed-down
;; 	(gated B
;; 	       (randomly
;; 		(hazard) 
;; 		(gated A
;; 		       (bricks C)) 
;; 		(hazard) 
;; 		(bricks A)))
;; 	(with-fortification
;; 	    (with-bulkheads
;; 		(gated C
;; 		       (mixed-up
;; 			(skewed (hazard) (bricks B))
;; 			(hazard)
;; 			(bordered
;; 			 (mixed-down 
;; 			  (gated A (bricks (or *required-color* B)))
;; 			  (hazard)
;; 			  (mixed-up (hazard)
;; 				    (bricks A))))
;; 			(let ((*puzzle-border* 14))
;; 			  (mixed-down (wildcard)
;; 				      (make-exit (derange (theme-colors)))))))))
;; 	;;
;; 	(mixed-down
;; 	 (skewed (bricks B) (hazard) (bricks C))
;; 	 (gated B (randomly
;; 		   (skewed (hazard)
;; 			   (bricks A)
;; 			   (hazard))
;; 		   (mixed-up
;; 		    (bricks C)
;; 		    (bricks B)
;; 		    (hazard)
;; 		    (bricks (random-color)))))))))))

  
(defun make-puzzle-4 (colors)
  (assert (= 4 (length colors)))
  (let ((key (random-choose colors)))
    (destructuring-bind (A B C D) 
	(derange colors)
      (destructuring-bind (P Q R) 
	  (requiring key
	    (puzzle-3-components (list A B C)))
	(let ((sector-1 
		(skewed
		 (bricks (random-color))
		 (bricks C)
		 (hazard)
		 (bricks key)
		 (hazard)
		 (bricks D)))
	      (sector-2 
		(mixed-down 	 
		  (gated A 
			 (mixed-up 
			  (hazard)
			  (bricks C)
			  (bricks key)
			  (hazard)))
		  (skewed (bricks B)
			  (hazard)
			  (bricks A)
			  (bricks D))
		  (hazard))))
	  (percent-of-time 50 (rotatef sector-1 sector-2))
	  (laid-out 
	   sector-1
	   (gated C
		  (mixed-down 		 
		   (gated B P)
		   (with-bulkheads
		       (with-fortification
			   (mixed-down (bricks key)
				       (hazard)
				       R
				       (bricks (random-color)))))))
	   (with-bulkheads
	       (gated key
		      (with-fortification Q)))
	   sector-2))))))

(defun make-reactor-level ()
  (stacked-up 
   (with-automatic-padding (singleton (new 'barrier)))
   (themed-row-buffer 30)
   (with-automatic-padding (singleton (new 'barrier)))
   (with-bulkheads
       (stacked-up
	(themed-row-buffer 30)
	 (singleton (new 'barrier))
	(singleton (new 'barrier))
	(singleton (new 'reactor))
	(themed-row-buffer 30)
	(singleton (new 'barrier))
	(singleton (new 'barrier))))
   (with-automatic-padding (singleton (new 'barrier)))
   (with-automatic-padding (singleton (new 'exit)))))
  
(defun make-puzzle (colors)
  (assert (every #'stringp colors))
  (let ((*depth* (random 2)))
    (case (length colors) 
      (2 (make-puzzle-2 colors))
      (3 (make-puzzle-3 colors))
      (4 (with-fortification (make-puzzle-4b colors)))
      (5 (with-bulkheads (make-reactor-level))))))

(defun pad-to-window (buffer)
  (prog1 buffer
    (with-fields (width height) buffer
      (setf width (max width *screen-width*))
      (setf height (max height *screen-height*)))))

(defun 2x0ng-level (&optional (level 1))
  (configure-level level)
  (setf *ball* nil)
  (let ((robot (new 'player-1-robot :color "gold"))
	(buffer (new '2x0ng))
	(puzzle (pad-to-window
		 (with-border (units 8)
		   (make-puzzle (derange (level-colors)))))))
    (with-buffer buffer
      (setf (%background-color (current-buffer)) (background-color))
      ;;
      (paste-from buffer puzzle)
      ;; playfield border
      (wall-around-region -1 2 
			  (+ 8 (truncate (/ (%width puzzle) (units 1))))
			  (+ 8 (1- (truncate (/ (%height puzzle)
						 (units 1))))))
      (destroy puzzle)
      ;; adjust scrolling parameters 
      (setf (%window-scrolling-speed buffer) (/ *robot-speed* 2)
	    (%horizontal-scrolling-margin buffer) 2/5
	    (%vertical-scrolling-margin buffer) 4/7)
      ;;
      (trim (current-buffer))
      ;; player 1 always goes top left on level 1
      (if (or (= *level* 1)
	      ;; otherwise, randomly choose a corner
	      (percent-of-time 50 t))
	  (drop-object (current-buffer) robot (units 4) (units 3.4))
	  (drop-object (current-buffer) robot 
		       (- (%width (current-buffer))
			  (units 12)) 
		       (- (%height (current-buffer))
			  (units 8))))
      (set-cursor (current-buffer) robot)
      (snap-window-to-cursor (current-buffer))
      (glide-window-to-cursor (current-buffer))
      ;; allocate
      (install-quadtree (current-buffer))
      ;; bugfix
      (follow-with-camera (current-buffer) robot)
      (raise-shield robot)
      ;; maybe show greeting or level number
      (if (= *level* 1)
	  (drop-object (current-buffer) (new 'greeting) (units 8) (units 2.8))
	  (drop robot
	  	(new 'bubble :text (format nil "LEVEL: ~S        RETRIES: ~S " *level* *retries*) :font
	  	     "sans-mono-bold-16")))
      (when (or (null *music-toggled*) 
      		(sdl-mixer:music-playing-p))
      	(play-music (random-choose (level-music)) :loop t))
      (current-buffer))))

