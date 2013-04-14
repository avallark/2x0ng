(in-package :2x0ng)

(defun glitchp (thing)
  (and (blockyp thing)
       (has-tag thing :glitch)))

(defun robotp (thing)
  (and (blockyp thing)
       (has-tag thing :robot)))

(defun bubblep (thing)
  (and (blockyp thing)
       (has-tag thing :bubble)))

(define-block bubble 
  (tags :initform '(:bubble))
  (text :initform nil) 
  (collision-type :initform nil))

(define-method initialize bubble (text &optional (font "sans-mono-bold-16"))
  (setf %text text)
  (setf %font font)
  (later 12.0 (destroy self)))

(define-method draw bubble ()
  (multiple-value-bind (top left right bottom)
      (window-bounding-box (current-buffer))
    (draw-box left top (- right left) (units 2) :color "black")
    (draw-string %text 
		 (+ left (units 1))
		 (+ top (units 0.4))
		 :color (random-choose '("cyan" "white"))
		 :font %font)))

(define-method update bubble ()
  (when (cursor)
    (multiple-value-bind (x y)
	(right-of (cursor))
      (move-to self x y))))

(defun targetp (thing)
  (and (blockyp thing)
       (has-tag thing :target)))

(defun enemyp (thing)
  (and (blockyp thing)
       (has-tag thing :enemy)))

(defresource "go.wav" :volume 60)
(defresource "shield.wav" :volume 60)
(defresource "corruption-horz2.png")
(defresource "corruption-horz.png")
(defresource "drone.png")
(defresource "osc-sine-off.png")
(defresource "pulsator-pulse.png")
(defresource "pulsator-on.png")
(defresource "resonator-on.png")
(defresource "resonator.png")
(defresource "pulsator.png")
(defresource "reactor1.png")
(defresource "reactor2.png")
(defresource "reactor3.png")
(defresource "reactor4.png")
(defresource "reactor5.png")
(defresource "reactor6.png")
(defresource "reactor7.png")
(defresource "reactor8.png")
(defresource "reactor9.png")
(defresource "shocker.png")
(defresource "shocker2.png")
(defresource "waypoint.png")
(defresource "waypoint2.png")
(defresource "tank-disabled.png")
(defresource "tank-northeast.png")
(defresource "tank-north.png")
(defresource "trigger.png")
(defresource "turret-right-on.png")
(defresource "turret-right.png")
(defresource "woom.wav" :volume 20)
(defresource "alarm.wav" :volume 30)
(defresource "sense.wav" :volume 10)
(defresource "sense2.wav" :volume 10)

(defresource 
    (:name "xplod.wav"
     :type :sample :file "xplod.wav" 
     :properties (:volume 30)))

(defresource 
    (:name "robovoxx.wav"
     :type :sample :file "robovoxx.wav" 
     :properties (:volume 30)))

(defresource 
      (:name "boop1.wav" :type :sample :file "boop1.wav" :properties (:volume 20))
      (:name "boop2.wav" :type :sample :file "boop2.wav" :properties (:volume 20))
    (:name "boop3.wav" :type :sample :file "boop3.wav" :properties (:volume 20)))

(defparameter *bounce-sounds* '("boop1.wav" "boop2.wav" "boop3.wav"))

(defresource 
    (:name "doorbell1.wav" :type :sample :file "doorbell1.wav" :properties (:volume 23))
    (:name "doorbell2.wav" :type :sample :file "doorbell2.wav" :properties (:volume 23))
  (:name "doorbell3.wav" :type :sample :file "doorbell3.wav" :properties (:volume 23)))

(defparameter *doorbell-sounds* '("doorbell1.wav" "doorbell2.wav" "doorbell3.wav"))

(defparameter *slam-sounds*
  (defresource 
      (:name "slam1.wav" :type :sample :file "slam1.wav" :properties (:volume 52))
      (:name "slam2.wav" :type :sample :file "slam2.wav" :properties (:volume 52))
    (:name "slam3.wav" :type :sample :file "slam3.wav" :properties (:volume 52))))

(defresource 
    (:name "whack1.wav" :type :sample :file "whack1.wav" :properties (:volume 82))
    (:name "whack2.wav" :type :sample :file "whack2.wav" :properties (:volume 82))
  (:name "whack3.wav" :type :sample :file "whack3.wav" :properties (:volume 82)))

(defparameter *whack-sounds* '("whack1.wav" "whack2.wav" "whack3.wav"))

(defresource 
    (:name "color1.wav" :type :sample :file "color1.wav" :properties (:volume 32))
    (:name "color2.wav" :type :sample :file "color2.wav" :properties (:volume 32))
  (:name "color3.wav" :type :sample :file "color3.wav" :properties (:volume 32)))

(defparameter *color-sounds* '("color1.wav" "color2.wav" "color3.wav"))

(defresource 
    (:name "bonux1.wav" :type :sample :file "bonux1.wav" :properties (:volume 22))
    (:name "bonux2.wav" :type :sample :file "bonux2.wav" :properties (:volume 22))
  (:name "bonux3.wav" :type :sample :file "bonux3.wav" :properties (:volume 22)))

(defparameter *bonux-sounds* '("bonux1.wav" "bonux2.wav" "bonux3.wav"))

;;; Sparkle explosion cloud fx

(define-block spark 
  :width 3 :height 3 :color nil)
;  :collision-type nil)

(define-method collide spark (thing)
  (when (and (enemyp thing) (has-method :damage thing))
    (damage thing 1)
    (destroy self)))

(define-method initialize spark (color)
  (setf %color color)
  (later 0.3 (destroy self)))

(define-method draw spark ()
  (set-blending-mode :additive)
  (with-field-values (x y color) self
    (dotimes (n 8)
      (let ((z (+ 2 (random 4))))
	(draw-box (+ x (- 20 (random 40)))
		  (+ y (- 20 (random 40)))
		  z z
		  :color (random-choose '("cyan" "magenta"))))))
  (set-blending-mode :alpha))

(define-method update spark ()
  (move-toward self (random-direction) (+ 8 (random 8))))

(defun make-sparks (x y &optional color)
  (dotimes (z 5)
    (drop-object (current-buffer)
		 (new 'spark color) 
		 (+ x (random 30)) (+ y (random 30)))))

;;; Versatile bullets

(defun bulletp (thing)
  (has-tag thing :bullet))

(defun player-bullet-p (thing)
  (and (bulletp thing)
       (has-tag thing :player)))

(defun enemy-bullet-p (thing)
  (and (bulletp thing)
       (not (player-bullet-p thing))))

(define-block bullet 
  :radius 3
  :speed 7.2
  :timer 200
  :blend :alpha
  :growth-rate nil
  :tags '(:bullet))

(defresource "bullet.png")
(defresource "bullet2.png")
(defresource "bullet3.png")

(define-method draw bullet ()
  (draw-textured-rectangle 
   %x %y %z 8 8 (find-texture (random-choose '("bullet.png" "bullet2.png" "bullet3.png")))
	       :vertex-color (random-choose 
		       (if (player-bullet-p self)
			   '("green" "yellow")
			   '("cyan" "white" "deep sky blue")))))
	      
(define-method update bullet ()
  (decf %timer)
  (if (zerop %timer) 
      (destroy self)
      (forward self %speed)))

(define-method collide bullet (thing)
  (cond 
    ((brickp thing)
     (destroy self))
    ((barrierp thing)
     (destroy self))
    ;; let bullets pass through clouds
    ((has-tag thing :cloud)
     nil)
    ;; ;; let enemy bullets pass through barriers
    ;; ((and (is-barrier thing)
    ;; 	  (enemy-bullet-p self))
    ;;  nil)
    ;; don't get hung up on collectibles
    ;; ((or (is-powerup thing)
    ;; 	 (is-chip thing))
    ;;  nil)
    ;; hit enemies with player bullets
    ((and (player-bullet-p self)
	  (enemyp thing))
     (damage thing 1)
     (destroy self))
    ;; allow player bullets to pass through trail
    ;; (and through the player)
    ((and (player-bullet-p self)
	  (or (trailp thing)
	      (player-bullet-p thing)
	      (robotp thing)))
     nil)
    ;; enemy bullets don't hurt enemies
    ;; or other enemy bullets
    ((or (enemyp thing)
	 (enemy-bullet-p thing))
     nil)
    ;; player bullets do not hurt enemy bullets
    ((or 
      (and (player-bullet-p self)
	   (enemy-bullet-p thing))
      (and (enemy-bullet-p self)
	   (player-bullet-p thing)))
     nil)
    ;; kill player
    ((and (enemy-bullet-p self)
	  (robotp thing))
     (die thing))
    ;; by default, just damage whatever it is
    (t (when (has-method :damage thing)
	 (damage thing 1)
	 (destroy self)))))

(define-method initialize bullet (heading &key tags speed radius timer)
  (initialize%super self)
  (setf %heading heading)
  (when speed (setf %speed speed))
  (when timer (setf %timer timer))
  (when radius (setf %radius radius))
  (setf %height 
	(setf %width
	      (* 1.8 %radius)))
  (when tags
    (dolist (tag tags)
      (add-tag self tag))))

;; Breakable colored bricks

(define-block brick 
  :tags '(:brick :colored)
  :collision-type :passive
  :color "gray50")

(define-method damage brick (points) 
  (multiple-value-bind (x y) (center-point self)
    (make-sparks x y %color)
    (destroy self)))

(define-method initialize brick (&optional color)
  (initialize%super self)
  (resize self (units 3) (units 2))
  (when color (setf %color color)))

(define-method draw brick ()
  (with-fields (x y z width height color) self
    (let ((hash (color-hash color)))
      (set-blending-mode :alpha)
      (if (and *red-green-color-blindness* hash)
	  (draw-textured-rectangle x y z width height
				   (find-texture (solid-hash-image hash))
				   :vertex-color color)
	  (draw-box x y width height :color color)))))

(defun slap (thing)
  (when (and (blockyp thing)
	     (has-method :damage thing))
    (damage thing 1)))

(defun brickp (thing)
  (and (blockyp thing)
       (has-tag thing :brick)))

(defun coloredp (thing)
  (and (blockyp thing)
       (has-tag thing :colored)))

(defun color-of (thing)
  (or (when (coloredp thing)
	(%color thing))
      "white"))

(define-method paint brick (color)
  (when (coloredp self)
    (setf %color color)))

(defun same-color (a b)
  (string= (color-of a) (color-of b)))

;;; A non-breakable brick

(defparameter *wall-color* "gray50")

(define-block (wall :super brick)
  (tags :initform '(:brick :wall))
  (color :initform (wall-color)))

(define-method initialize wall (&optional width height)
  (when (and (numberp height) (numberp width))
    (resize self width height)))

(defun wallp (thing)
  (and (blockyp thing)
       (has-tag thing :wall)))

(define-method damage wall (points) 
  ;; TODO flash and/or make a cool (themeable) sound 
  nil)

;;; A colored block that only opens with the right color

(defresource "gate.png")
(defresource "gate2.png")
(defresource "gate3.png")
(defresource "gate-closing-sound.wav" :volume 150)

(defresource "hash1.png")
(defresource "hash2.png")
(defresource "hash3.png")
(defresource "hash4.png")

(defun ball-hash-image (n)
  (ecase n
    (1 "hash1.png")
    (2 "hash2.png")
    (3 "hash3.png")
    (4 "hash4.png")))

(defresource "gate1-hash1.png")
(defresource "gate1-hash2.png")
(defresource "gate1-hash3.png")
(defresource "gate1-hash4.png")
(defresource "gate2-hash1.png")
(defresource "gate2-hash2.png")
(defresource "gate2-hash3.png")
(defresource "gate2-hash4.png")
(defresource "brick-hash1.png")
(defresource "brick-hash2.png")
(defresource "brick-hash3.png")
(defresource "brick-hash4.png")

(defun hash-image (n)
  (if *red-green-color-blindness*
      (ecase n
	(1 "gate1-hash1.png")
	(2 "gate1-hash2.png")
	(3 "gate1-hash3.png")
	(4 "gate1-hash4.png"))
      "gate.png"))

(defun large-hash-image (n)
  (if *red-green-color-blindness*
      (ecase n
	(1 "gate2-hash1.png")
	(2 "gate2-hash2.png")
	(3 "gate2-hash3.png")
	(4 "gate2-hash4.png"))
      "gate2.png"))

(defun solid-hash-image (n)
  (or (when *red-green-color-blindness*
	(case n
	  (1 "brick-hash1.png")
	  (2 "brick-hash2.png")
	  (3 "brick-hash3.png")
	  (4 "brick-hash4.png")))
      "gate3.png"))

(defresource "error.wav" :volume 70)

(defun gatep (thing)
  (and (blockyp thing)
       (has-tag thing :gate)))

(define-block (gate :super brick)
  (tags :initform '(:brick :colored :gate))
  (color :initform "white"))

(define-method draw gate ()
  (with-fields (x y z width height color) self
    (draw-textured-rectangle x y z width height 
			     (find-texture (hash-image (color-hash color)))
			     :vertex-color color)
    (when (> (max height width) 700)
      (draw-textured-rectangle x y z width height 
			       (find-texture (large-hash-image (color-hash color)))
			       :vertex-color color))))

(define-method paint gate (color)
  (if (not (string= %color color))
      (play-sample "error.wav")
      (progn 
	(play-sample "gate-closing-sound.wav")
	(destroy self))))

(define-method unlock gate ()
  (play-sample (random-choose *doorbell-sounds*))
  (destroy self))

(define-method damage gate (points))

;;; The football

(defvar *ball* nil)

(defvar *auto-return-distance* 520)

(defparameter *ball-size* (truncate (units 1.2)))

(defun ballp (thing)
  (and (blockyp thing)
       (has-tag thing :ball)))

(defparameter *ball-normal-speed* (units 0.88))

(defparameter *ball-kick-speed* (units 1.16))

(defparameter *ball-deceleration* (units 0.0))

(define-block ball 
  :kicker nil
  :target nil
  :seeking nil
  :height *ball-size* :width *ball-size*
  :color "white"
  :speed 0
  :bounces 20
  :hits 7
  :kick-clock 0 :tags '(:ball :colored))

(define-method paint ball (color)
  ;; beep when color changes
  (when (not (string= %color color))
    (play-sample (random-choose *color-sounds*)))
  (setf %color color))

(define-method initialize ball (&optional color)
  (initialize%super self)
;  (report-database)
  (setf *ball* self)
  (resize self *ball-size* *ball-size*)
  (when color (setf %color color)))

(defun make-ball (&optional (color "white"))
  (setf *ball* (new 'ball color)))

(define-method after-deserialize ball ()
  (after-deserialize%super self)
  (setf *ball* self))

(defresource "bounce.wav" :volume 10)
(defresource "newball.wav" :volume 20)
(defresource "return.wav" :volume 20)

(define-method bounce ball ()
  (decf %bounces)
  (if (zerop %bounces)
      (progn (make-sparks %x %y "white")
	     (setf *ball* nil)
	     (paint %kicker %color)
	     (play-sample "error.wav")
	     (destroy self))
      (progn
	(play-sound self (random-choose *bounce-sounds*))
	(unless (zerop %last-x)
	  (restore-location self))
	(setf %heading 
	      (if %seeking
		  (opposite-heading %heading)
		  (opposite-heading %heading)))
	(move self %heading 2.2)
	(setf %kick-clock 0)
	(setf %seeking (if %seeking nil t)))))
      
(define-method find-enemy ball (thing2 &optional (range 180))
  (let ((enemies
	  (loop for thing being the hash-values of (%objects (current-buffer))
		when (and 
		      (blockyp thing)
		      (enemyp thing)
		      (has-tag thing :target)
		      (not (trailp thing))
		      (not (object-eq thing2 thing))
		      (colliding-with-rectangle thing 
						(- %y range)
						(- %x range)
						(* 2 range)
						(* 2 range)))
		  collect thing)))
    (when (consp enemies)
      (first enemies))))

(define-method update ball ()
  (when (not (blockyp %target)) (setf %target nil))
  (when (plusp %kick-clock)
    (decf %kick-clock))
  (with-fields (seeking heading speed kicker) self
    (when (> (distance-between self kicker) *auto-return-distance*)
      (when (not %seeking) (play-sample "return.wav"))
      (setf %seeking t))
    (if (plusp speed)
	(if %target 
	    (move self (heading-to-thing self %target) speed)
	    (if seeking
		(move self (heading-to-thing self kicker) speed)
		(move self heading speed)))
	(setf %seeking nil))))

(define-method recently-kicked-by ball (cursor)
  (and (plusp %kick-clock) 
       (object-eq cursor %kicker)))
    
(define-method collide ball (thing)
  (cond 
    ((glitchp thing)
     (setf *ball* nil)
     (play-sample "error.wav")
     (destroy self))
    ;; unlock gates
    ((gatep thing)
     (setf %target nil)
     (when (same-color self thing)
       (unlock thing))
     (bounce self))
    ;; target and smash enemies
    ((and (enemyp thing) (not (trailp thing)))
     (decf %hits)
     (when (has-method :damage thing)
       (damage thing 1))
     (setf %target
	   (when (plusp %hits)
	     (find-enemy self thing)))
     (when %target (setf %seeking nil))
     (when (zerop %hits) (setf %seeking t))
     (bounce self))
    ;; stop at robot unless it's the guy who just kicked it.
    ;; helps avoid ball getting stuck.
    ((and (robotp thing) (humanp thing))
     ;; player catches ball
     (when (not (recently-kicked-by self thing))
       (paint thing (color-of *ball*))
       (destroy self)
       (setf *ball* nil)))
    ;; enemy AI catches ball 
    ((and (robotp thing) (not (humanp thing)))
     (when
	 (and (field-value :alive thing)
	      (not (field-value :carrying thing)))
       (play-sample "alarm.wav")
       (drop-object (current-buffer) 
		    (new 'bubble "I GOT THE BALL!!" "sans-mono-bold-20")
		    %x %y))
     (setf (field-value :carrying thing) t))
    ;; barriers
    ((barrierp thing)
     (bounce self))
    ;; bounce off bricks
    ((brickp thing)
     (setf %target nil)
     (when (coloredp thing)
       (paint self (color-of thing)))
     (slap thing)
     (bounce self))))

(defparameter *ball-kicker-collision-delay* 4)

(define-method impel ball (heading strong &optional kicker)
  (setf %kicker kicker)
  (setf %kick-clock *ball-kicker-collision-delay*)
  (setf %seeking nil)
  (setf %speed (if strong *ball-kick-speed* *ball-normal-speed*))
  (setf %heading heading))

(define-method draw ball ()
  (with-field-values (x y width height color) self
    (draw-box x y width height :color "white")
    (draw-box (+ 2 x) (+ 2 y) (- width 4) (- height 4) :color color)
    (when *red-green-color-blindness*
      (let ((hash (color-hash color)))
	(when hash
	  (draw-textured-rectangle x y 0 width height 
				   (find-texture (ball-hash-image hash))))))))

;;; Level exit

(defresource "exit1.png")
(defresource "exit2.png")

(define-block exit
  (open :initform nil)
  (image :initform "exit2.png"))

(defun bossp (thing)
  (and (blockyp thing)
       (has-tag thing :boss)))

(defun boss-remaining-p ()
  (loop for thing being the hash-keys of (%objects (current-buffer))
	when (bossp thing) return thing))

(define-method update exit ()
  (with-fields (image open) self
    (if (boss-remaining-p) 
	(setf open nil image "exit2.png")
	(setf open t image "exit1.png"))))

(defun find-exit ()
  (loop for thing being the hash-keys of (%objects (current-buffer))
	when (exitp thing) return thing))

(defun exitp (thing) (is-a 'exit thing))

(defun exit-open-p ()
  (let ((exit (find-exit)))
    (and exit (%open exit))))
