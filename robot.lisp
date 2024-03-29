(in-package :2x0ng)

(defparameter *initial-retries* 2)

(defparameter *retries* *initial-retries*)

;; ;;; Dialogue

;; (defresource "robot-talk-1.png")
;; (defresource "robot-talk-2.png")
;; (defresource "robot-talk-3.png")
;; (defresource "robot-talk-4.png")

;; (defparameter *talking-images* 
;;   '("robot-talk-1.png" "robot-talk-2.png" "robot-talk-3.png" "robot-talk-4.png")) 

;; (defresource "balloon.png")
;; (defresource "balloon2.png")

;; (defparameter *balloon-images* 
;;   '("balloon.png" "balloon2.png"))

;; (defvar *dialogue* nil)

;; (defvar *actor* nil)

;; (defvar *dialogue-channel* nil)

;; (defun talkingp (thing) 
;;   (and (robotp thing)
;;        (field-value :talking thing))) 

;; (defun robot-talk-image ()
;;   (random-choose *talking-images*))

;; (defun dialogue-playing-p () 
;;   (and *dialogue* (integerp *dialogue-channel*)))

;; (defun say (actor line) 
;;   (setf *dialogue*
;; 	(append *dialogue* (list (list actor line)))))

;; (defun act (actor method) 
;;   (setf *dialogue*
;; 	(append *dialogue* (list (list actor method)))))

;; (defun stop-dialogue ()
;;   (setf *dialogue* nil)
;;   (halt-sample *dialogue-channel*)
;;   (setf *dialogue-channel* nil)
;;   (setf *actor* nil))

;; (defun play-dialogue ()
;;   (if (null *dialogue*)
;;       (stop-dialogue)
;;       (destructuring-bind (actor line) (pop *dialogue*)
;; 	(when (xelfp *actor*)
;; 	  (stop-talking *actor*))
;; 	(setf *actor* actor)
;; 	;; is it an audio line or an action?
;; 	(etypecase line
;; 	  (string 
;; 	   (begin-talking *actor* line)
;; 	   (setf *dialogue-channel* 
;; 		 (play-sample line)))
;; 	  (keyword 
;; 	   (send line *actor*))))))

;; (defun update-dialogue ()
;;   (when (and (dialogue-playing-p)
;; 	     (not (sdl-mixer:sample-playing-p *dialogue-channel*)))
;;     (play-dialogue)))

;;; Waypoint 

(defblock waypoint :image "waypoint.png" :counter 35 :collision-type :passive)

(defparameter *waypoint-interval* (seconds->frames 12))

(defparameter *waypoint-distance* 800)

(defparameter *waypoint-clock* 0)

(define-method update waypoint ()
  (percent-of-time 30 (setf %image (random-choose '("waypoint.png" "waypoint2.png"))))
  (decf %counter)
  (if (zerop %counter)
      (destroy self)
      (let ((exit (find-exit)))
	(if (xelfp exit)
	    (multiple-value-bind (x y) 
		(step-toward-heading (cursor)
				     (heading-to-thing (cursor) exit)
				     280)
		(move-to self x y))
	    (destroy self)))))

;;; A player, either AI or human controlled

(defblock robot 
  (leave-direction :initform nil)
  (alive :initform t)
  (talking :initform nil)
  (shielded :initform nil)
  (shield-clock :initform 0)
  (hearing-distance :initform 650)
  (body-color :initform "white")
  (color :initform "white")
  (carrying :initform nil)
  (tags :initform '(:robot :colored))
  (direction :initform :up)
  (kick-direction :initform :up)
  ;; timers
  (retry-clock :initform (seconds->frames 5))
  (walk-clock :initform 0)
  (step-clock :initform 0)
  (kick-clock :initform 0))
  ;; ;; we want to catch the beginning of firing, even if the input
  ;; ;; polling in `update' misses it. (see below)
  ;; (default-events :initform '(((:space) (strong-kick)))))

(defparameter *robot-colors* '("gold" "olive drab" "RoyalBlue3" "dark orchid"))

(defmethod initialize :after ((self robot) &key color)
  (when color (setf (field-value :body-color self) color)))

(define-method raise-shield robot ()
  (play-sound self "go.wav")
  (setf %shielded t %shield-clock (seconds->frames 3)))

(define-method humanp robot () nil)

(defvar *ball-carrier* nil)

;;; Drawing the robot onscreen and animating his feet

(defparameter *robot-step-frames* 2)

(defparameter *robot-empty-color* "white")

(defparameter *robot-speed* (truncate (/ *unit* 1.3)))

(defparameter *robot-reload-frames* 10)

(defparameter *walk-interval* 16)

(define-method animate-walk robot ()
  (with-fields (walk-clock step-clock) self
    ;; only when moving
    (when (plusp step-clock)
      (when (plusp walk-clock)
	(decf walk-clock))
      (when (zerop walk-clock)
	(setf walk-clock *walk-interval*)))))

(defresource "robot-right.png")
(defresource "robot-right-lstep.png")
(defresource "robot-right.png")
(defresource "robot-right-rstep.png")

(define-method leave robot ()
  (setf %leave-direction :down))

(defparameter *walking-right* 
  '("robot-right.png"
    "robot-right-lstep.png"
    "robot-right.png"
    "robot-right-rstep.png"))

(defresource "robot-up.png")
(defresource "robot-up-lstep.png")
(defresource "robot-up.png")
(defresource "robot-up-rstep.png")

(defparameter *walking-up* 
  '("robot-up.png"
    "robot-up-lstep.png"
    "robot-up.png"
    "robot-up-rstep.png"))
        
(defun robot-image (direction clock)
  (let ((frames
	  (or 
	   (cond
	     ((member direction '(:up :down :upright :downleft))
	      *walking-up*)
	     ((member direction '(:left :right :upleft :downright))
	      *walking-right*))
	   *walking-up*)))
    (or (nth (truncate (* clock (/ 4 *walk-interval*)))
	     frames)
	(if (or (eq direction :left) (eq direction :right))
	    "robot-right.png"
	    "robot-up.png"))))

(define-method begin-talking robot (line)
  (setf %talking t))

(define-method stop-talking robot ()
  (setf %talking nil))

(define-method serve-location robot ()
  (with-fields (direction) self
    (multiple-value-bind (cx cy) (center-point self)
      (multiple-value-bind (tx ty) 
	  (step-in-direction cx cy direction (units 0.7))
	(values (- tx (* *ball-size* 0.4))
		(- ty (* *ball-size* 0.4)))))))

(define-method draw robot ()
  (let ((image 
	  (if %alive
	      (or
	       (when %talking (robot-talk-image))
	       (robot-image %direction %walk-clock) 
	       "robot-up.png")
	      "skull.png")))
    (draw-textured-rectangle %x %y %z %width %height (find-texture image) 
			     :vertex-color %body-color)
    (when %shielded 
      (multiple-value-bind (cx cy) (center-point self)
	(draw-circle cx cy 30 :color (random-choose '("deep pink" "yellow")))))
    (when %talking
      (multiple-value-bind (bx by) (right-of self)
	(draw-image (random-choose '("balloon.png" "balloon2.png"))
		    bx (- by (units 2)))))))
	  
(define-method serve robot ()
  (multiple-value-bind (x y) (serve-location self)
    (make-ball %color)
    (drop-object (current-buffer) *ball* x y)))

;;; Cool vintage footstep and kick sounds

(defresource "left-foot.wav" :volume 70)
(defresource "right-foot.wav" :volume 70)

(define-method footstep-sound robot ()
  (case %walk-clock
    ;; on first step
    (0 "left-foot.wav")
    ;; on 8th steps while looping 
    (1 "left-foot.wav")
    (9 "right-foot.wav")))

(defparameter *footstep-sound-range* 300)

(define-method make-footstep-sounds robot ()
  (let ((sound (footstep-sound self)))
    (when sound 
      (when (< (distance-to-cursor self) 400)
	(play-sound self sound)))))

(defresource "kick.wav" :volume 23)
(defresource "serve.wav" :volume 23)

(defparameter *kick-sound* "kick.wav")

;;; Default AI methods. 

(define-method movement-direction robot () 
  (if %carrying
      (opposite-direction (direction-to-cursor self))
      (if (< (distance-to-cursor self) 400)
  	  (if *ball*
  	      (direction-to-thing self *ball*)
  	      (if (> (distance-to-cursor self) 250)
  		  (opposite-direction (direction-to-cursor self))
  		  (or (percent-of-time 3 (setf %direction (random-choose *directions*)))
  		      %direction))))))

(defvar *joystick-enabled* nil)

(define-method stick-heading robot () 
  (when (humanp self)
    (or (when (left-analog-stick-pressed-p)
	  (left-analog-stick-heading))
	(when (right-analog-stick-pressed-p)
	  (right-analog-stick-heading)))))

(define-method can-reach-ball robot ()
  (and *ball* (colliding-with self *ball*)))

(define-method bounding-box robot ()
  ;; shrink bounding box by a few px to make game more forgiving
  (with-field-values (x y height width) self
    (let ((margin 2))
      (values (+ margin y) (+ margin x)
	      (+ (- margin) x width)
	      (+ (- margin) y height)))))

(define-method collide robot (thing)
  (when (or (brickp thing) (enemyp thing) (holep thing) (cloudp thing))
    (restore-location self)))

(defresource "skull.png")

(defresource "analog-death.wav" :volume 70)

(define-method die robot ()
  (when (and %alive (not %shielded))
    (when (humanp self) 
      (play-sample "analog-death.wav")
      (play-music "nexttime.ogg")
      (drop-object (current-buffer) 
		   (new 'bubble :text
		   	(if (= *retries* 0)
		   	    (format nil "You died on level ~A. GAME OVER. Press Control-R to reset at level 1." *level*)
		   	    (format nil "You died on level ~A. You have ~A retries remaining. Retrying..." *level* *retries*))
		   	:font "sans-mono-bold-16")))
    (make-sparks %x %y %color)
    (change-image self "skull.png")
    (setf %alive nil)))

(define-method damage robot (points) (die self))

(define-method strong-kick-p robot () nil)

(define-method kick robot (&optional direction strong)
  (when %alive
    (when (and (null *ball*) (zerop %kick-clock))
      (serve self)
      (impel *ball* 
	     (direction-heading (or direction %direction))
	     strong self)
      (setf *ball-carrier* self)
      (play-sound self "serve.wav")
      (setf %kick-clock *robot-reload-frames*))))

(define-method strong-kick robot ()
  (kick self nil t))

(define-method paint robot (color)
  (setf %color color))

;;; Control logic driven by the above (possibly overridden) methods.

(define-method drop-waypoint-maybe robot (&optional force)
  ;; possibly show waypoint
  (let ((exit (find-exit)))
    (when (or force (and exit (> (distance-between exit (cursor)) *waypoint-distance*)))
	(drop self (new 'waypoint))
	(play-sound self "shield.wav")
	(setf *waypoint-clock* *waypoint-interval*))))

(define-method retry-maybe robot ()
  (when (and (not %alive)
	     (humanp self)
	     (zerop %retry-clock)
	     (plusp *retries*))
    (decf *retries*)
    (setf (field-value :retrying (current-buffer)) t)))

(define-method update robot ()
;;  (when (dialogue-playing-p) (update-dialogue))
  (decf *waypoint-clock*)
  (unless (plusp *waypoint-clock*)
    (drop-waypoint-maybe self))
  ;; possibly lower shields
  (when %shielded
    (decf %shield-clock)
    (unless (plusp %shield-clock)
      (setf %shielded nil)))
  ;; player took ball away
  (when (and %carrying (null *ball*))
    (setf %carrying nil)
    (play-sound self "xplod.wav")
    (later 2 (die self))
    (later 7 (destroy self)))
  ;; carry ball
  (when (and %carrying *ball*)
    (move-to *ball* %x %y))
  ;; auto retry timeout
  (when (not %alive)
    (if (plusp %retry-clock)
	(decf %retry-clock)
	(retry-maybe self)))
  ;; normal update
  (when %alive
    (resize self (* 2 *unit*) (* 2 *unit*))
    (with-fields (step-clock kick-clock) self
      (when (plusp step-clock)
	(decf step-clock))
      ;; find out what direction the AI or human wants to go
      (let ((direction (movement-direction self))
	    (kick-button (strong-kick-p self)))
	(when direction 
	  ;; move in the movement direction
	  (move-toward self direction (/ *robot-speed* 2))
	  (setf %direction direction)
	  ;; possibly kick, lock firing dir when held
	  (unless (holding-fire)
	    (setf %kick-direction direction)))
	(if direction
	    ;; controller is pushing in a direction
	    (when (zerop step-clock)
	    ;; don't animate on every frame
	      (setf step-clock *robot-step-frames*)
	      ;; possibly make footstep sounds
	      (make-footstep-sounds self))
	    ;; not pushing. allow movement immediately
	    (setf step-clock 0 %walk-clock 0))
	;; update animation
	(animate-walk self)
	;; delay between kicks
	(when (plusp kick-clock)
	  (decf kick-clock))
	;; ready to kick?
	(when (zerop kick-clock)
	  (when (and (null *ball*) kick-button)
	    ;; yes, do it
	    (kick self %kick-direction kick-button)))))))

(defblock (thief robot)
  (body-color :initform "deep pink"))

;; Player 1 drives the logic with the arrows/numpad and spacebar

(defblock (player-1-robot robot)
  (body-color :initform "white"))

(defun holding-space ()
  (keyboard-down-p :space))     

;; (defun holding-alt ()
;;   (or (keyboard-modifier-down-p :lalt)
;;       (keyboard-modifier-down-p :ralt)))


(defun holding-fire ()
  (or (holding-space)
      (holding-shift)
      (some #'joystick-button-pressed-p
	    '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))))

(define-method collide player-1-robot (thing)
  (when (and (exitp thing) 
	     (exit-open-p))
    (when (xelfp *ball*)
      (destroy *ball*)
      (setf *ball* nil))
;    (destroy self)
    (begin-game (1+ *level*)))
  (when (or (enemyp thing) (holep thing))
    (die self))
  (when (brickp thing)
    (restore-location self)))

(define-method draw player-1-robot ()
  (call-next-method)
  ;; possibly draw held ball 
  (when (and %alive (null *ball*))
    (multiple-value-bind (x y) (serve-location self)
      (let ((width *ball-size*)
	    (height *ball-size*))
	(draw-box x y width height :color "white")
	(draw-box (+ 2 x) (+ 2 y) (- width 4) (- height 4) :color %color)
	(when *red-green-color-blindness*
	  (let ((hash (color-hash %color)))
	    (when hash
	      (draw-textured-rectangle x y 0 width height 
				       (find-texture (ball-hash-image hash))))))))))


(define-method strong-kick-p player-1-robot ()
  (holding-fire))

(define-method humanp player-1-robot () t)

(defun holding-down-arrow ()
  (or (joystick-button-pressed-p :down)
      (keyboard-down-p :kp2)
      (keyboard-down-p :down)))

(defun holding-up-arrow ()
  (or (joystick-button-pressed-p :up)
      (keyboard-down-p :kp8)
      (keyboard-down-p :up)))

(defun holding-left-arrow ()
  (or (joystick-button-pressed-p :left)
      (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-right-arrow ()
  (or (joystick-button-pressed-p :right)
      (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(define-method movement-direction player-1-robot ()
  (or 
   (cond 
     ((and (holding-down-arrow) (holding-right-arrow)) :downright)
     ((and (holding-down-arrow) (holding-left-arrow)) :downleft)
     ((and (holding-up-arrow) (holding-right-arrow)) :upright)
     ((and (holding-up-arrow) (holding-left-arrow)) :upleft)
     ((holding-down-arrow) :down)
     ((holding-up-arrow) :up)
     ((holding-left-arrow) :left)
     ((holding-right-arrow) :right))
   (let ((heading (stick-heading self)))
     (when 
	 (and *joystick-enabled*
	      (or (left-analog-stick-pressed-p)
		  (right-analog-stick-pressed-p)))
       (or (heading-direction heading) :left)))))
  
;;; Drawing the robot

  ;; (let ((heading (stick-heading self)))
  ;;   (when heading
  ;;     (multiple-value-bind (x y)
  ;; 	  (step-toward-heading self heading 50)
  ;; 	(draw-line %x %y x y :color "magenta"))
  ;;     (multiple-value-bind (x y)
  ;; 	  (step-in-direction %x %y (or (heading-direction heading)
  ;; 				       :downleft)
  ;; 				       60)
  ;; 	(draw-line %x %y x y :color "cyan"))))
