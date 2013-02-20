(in-package :2x0ng)

;;; A player, either AI or human controlled

(define-block robot 
  (alive :initform t)
  (color :initform "white")
  (tags :initform '(:robot :colored))
  (direction :initform :up)
  ;; timers
  (walk-clock :initform 0)
  (step-clock :initform 0)
  (kick-clock :initform 0)
  ;; we want to catch the beginning of firing, even if the input
  ;; polling in `update' misses it. (see below)
  (default-events :initform '(((:space) (strong-kick)))))

(defparameter *robot-colors* '("gold" "olive drab" "RoyalBlue3" "dark orchid"))

(define-method initialize robot (&optional color)
  (initialize%super self)
  (when color (setf %color color)))

(defvar *ball-carrier* nil)

;;; Drawing the robot onscreen and animating his feet

(defparameter *robot-step-frames* 2)

(defparameter *robot-empty-color* "gray40")

(defparameter *robot-speed* (truncate (/ *unit* 2)))

(defparameter *robot-reload-frames* 20)

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

(define-method draw robot ()
  (let ((image (or (robot-image %direction %walk-clock) "robot-up.png")))
    (draw-textured-rectangle %x %y %z %width %height (find-texture image) 
			     :vertex-color (color-of self))))

;;; Cool vintage footstep and kick sounds

(defresource "left-foot.wav" :volume 60)
(defresource "right-foot.wav" :volume 60)

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
    (when (and *ball* sound 
	       (> *footstep-sound-range* 
		  ;; only make sound near the ball
		  (distance-between self *ball*)))
      (play-sound self sound))))

(defresource "kick.wav" :volume 15)
(defresource "serve.wav" :volume 15)

(defparameter *kick-sound* "kick.wav")

;;; Default AI methods. 

(define-method collide robot (thing)
  (when (brickp thing)
    (restore-location self)))

(define-method movement-direction robot ()
  (or (percent-of-time 5 (setf %direction (random-choose *directions*)))
      %direction))

(define-method can-reach-ball robot ()
  (and *ball* (colliding-with self *ball*)))

(define-method strong-kick-p robot () t)

(define-method kick robot (&optional direction strong)
  (when (and (zerop %kick-clock)
	     (can-reach-ball self))
    (impel *ball* (or direction %direction) strong self)
    (setf *ball-carrier* self)
    (play-sound self "serve.wav")
    (setf %kick-clock *robot-reload-frames*)))

(define-method strong-kick robot ()
  (kick self nil t))

;;; Control logic driven by the above (possibly overridden) methods.

(define-method update robot ()
  (resize self (* 2 *unit*) (* 2 *unit*))
  (with-fields (step-clock kick-clock) self
    (when %alive
      ;; don't move on every frame
      (when (plusp step-clock)
	(decf step-clock))
      ;; find out what direction the AI or human wants to go
      (let ((direction (movement-direction self))
	    (kick-button (strong-kick-p self)))
	(if direction
	    ;; controller is pushing in a direction
	    ;; don't move on every frame
	    (when (zerop step-clock)
	      (setf step-clock *robot-step-frames*)
	      ;; possibly make footstep sounds
	      (make-footstep-sounds self)
	      ;; move in the movement direction
	      (move-toward self direction *robot-speed*)
	      (setf %direction direction))
	    ;; not pushing. allow movement immediately
	    (setf step-clock 0 %walk-clock 0))
	;; update animation
	(animate-walk self)
	;; delay between kicks
	(when (plusp kick-clock)
	  (decf kick-clock))
	;; ready to kick?
	(when (zerop kick-clock)
	  ;; don't kick when standing still and not pressing button
	  (when (or direction kick-button)
	    (kick self direction kick-button)))))))

;;; Player 1 drives the logic with the arrows/numpad and spacebar

(define-block (player-1-robot :super robot)
  (color :initform "gold"))

(defun holding-space ()
  (keyboard-down-p :space))     

(define-method strong-kick-p player-1-robot ()
  (holding-space))

(defun holding-down-arrow ()
  (or (keyboard-down-p :kp2)
      (keyboard-down-p :down)))

(defun holding-up-arrow ()
  (or (keyboard-down-p :kp8)
      (keyboard-down-p :up)))

(defun holding-left-arrow ()
  (or (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-right-arrow ()
  (or (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(define-method movement-direction player-1-robot ()
  (cond 
    ((and (holding-down-arrow) (holding-right-arrow)) :downright)
    ((and (holding-down-arrow) (holding-left-arrow)) :downleft)
    ((and (holding-up-arrow) (holding-right-arrow)) :upright)
    ((and (holding-up-arrow) (holding-left-arrow)) :upleft)
    ((holding-down-arrow) :down)
    ((holding-up-arrow) :up)
    ((holding-left-arrow) :left)
    ((holding-right-arrow) :right)))

