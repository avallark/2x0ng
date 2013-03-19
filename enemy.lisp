(in-package :2x0ng)

(defun holep (thing)
  (and (blockyp thing)
       (has-tag thing :hole)))

;;; Positronic death filament

(defun trailp (thing)
  (and (blockyp thing)
       (has-tag thing :trail)))

(define-block trail
  (tags :initform '(:trail :enemy))
  (color :initform (random-choose '("cyan" "orchid" "magenta" "yellow")))
  (counter :initform 320)
  (collision-type :initform :passive)
  (height :initform 5)
  (width :initform 5))

(define-method draw trail ()
  (draw-box %x %y %width %height :color %color))

(define-method damage trail (points) (destroy self))

(define-method update trail ()
  (decf %counter)
  (when (zerop %counter) 
    (destroy self)))

;; Nasty tracers 

(defresource "tracer.png")

(define-block tracer
  (speed :initform 1.5)
  (tags :initform '(:enemy))
  (image :initform "tracer.png")
  (direction :initform (random-direction))
  (energy :initform 0))

(define-method lay-trail tracer ()
  (decf %energy)
  (unless (plusp %energy)
    (drop self (new 'trail) 6 6)
    (setf %energy 4)))

(define-method update tracer ()
  (percent-of-time 0.2 (setf %direction (random-direction)))
  (move-toward self %direction (with-difficulty 1.0 1.5 1.7 1.9 2.0 2.2 2.4 2.6 2.8))
  (lay-trail self))

(define-method collide tracer (thing)
  (when (brickp thing)
    (restore-location self)
    (setf %energy 6)
    (setf %direction (random-direction))))

(define-method damage tracer (points) nil)

;;; Electric death paddles!

(define-block paddle :tags '(:enemy :paddle) :heading (random-choose (list pi 0.0)))

(define-method initialize paddle ()
  (initialize%super self)
  (resize self 60 12))

(define-method draw paddle ()
  (draw-box %x %y %width %height :color (random-choose '("magenta" "deep pink" "hot pink"))))

(define-method update paddle ()
  (let ((speed
	  (if (< (distance-to-cursor self)
		 (with-difficulty 200 200 250 250 300 300 350))
	      (with-difficulty 8 8.5 9 9.5 10 10.5 11 11.5 12 12.5 13 13.5) 
	      (with-difficulty 2 2 3 3 4 4 5 6 7 8))))
    (forward self speed)))

(define-method collide paddle (thing)
  (when (brickp thing)
    (restore-location self)
    (setf %heading (- %heading pi))))

(define-method damage paddle (thing) nil)

;;; Radioactive corruption glitches that creep after you

(defresource 
    (:name "corruption1.png" :type :image :file "corruption1.png")
    (:name "corruption2.png" :type :image :file "corruption2.png")
  (:name "corruption3.png" :type :image :file "corruption3.png")
  (:name "corruption4.png" :type :image :file "corruption4.png")
  (:name "corruption5.png" :type :image :file "corruption5.png")
  (:name "corruption6.png" :type :image :file "corruption6.png"))

(defparameter *corruption-images*
'("corruption1.png" "corruption2.png" "corruption3.png" "corruption4.png" "corruption5.png" "corruption6.png"))

(defresource 
    (:name "pip1.wav" :type :sample :file "pip1.wav" :properties (:volume 40))
    (:name "pip2.wav" :type :sample :file "pip2.wav" :properties (:volume 40))
  (:name "pip3.wav" :type :sample :file "pip1.wav" :properties (:volume 40)))

(defparameter *corruption-sounds* '("pip1.wav" "pip2.wav" "pip3.wav"))

(defresource 
    (:name "blurp.wav" :type :sample :file "blurp.wav" :properties (:volume 40))
    (:name "blop.wav" :type :sample :file "blop.wav" :properties (:volume 40)))

(defparameter *glitch-sounds* 
  '("blurp.wav" "blop.wav"))

(defresource (:name "munch1.wav" :type :sample :file "munch1.wav" :properties (:volume 60)))
(defresource (:name "bigboom.wav" :type :sample :file "bigboom.wav" :properties (:volume 40)))

(define-block glitch
  (tags :initform '(:enemy :glitch))
  (clock :initform nil)
  (heading :initform (random (* pi 2)))
  (image :initform (random-choose *corruption-images*))
  (speed :initform (random-choose '(1 1.1 1.2)))
  (overlay-color :initform nil))

(define-method initialize glitch (&optional (depth 4))
  (setf %clock 30)
  (resize self 30 30)
  (initialize%super self)
  (setf %depth depth))

(define-method damage glitch (points)
  (play-sound self (random-choose *corruption-sounds*))
  (destroy self))

(define-method collide glitch (thing)
  (when (brickp thing)
    (restore-location self))
  (when (robotp thing)
    (die thing)))

(define-method set-overlay glitch ()
  (setf %overlay-color (random-choose '("cyan" "magenta" "yellow" "orange"))))

(define-method clear-overlay glitch ()
  (setf %overlay-color nil))

(define-method update glitch ()
  (move self %heading %speed)
  (incf %heading (radian-angle 0.8)) 
  (percent-of-time 6 
    (change-image self (random-choose *corruption-images*))))
  ;; (percent-of-time 3 
  ;;   (set-overlay self)
  ;;   (later 20 (clear-overlay self))))
      
(define-method draw glitch ()
  (draw%super self)
  (set-blending-mode :additive2)
  (when %overlay-color
    (draw-box %x %y %width %height
     :alpha 0.3
     :color %overlay-color)))

;;; The "monitor", a roving enemy 

(defresource
    (:name "monitor" :type :image :file "monitor.png")
    (:name "monitor2" :type :image :file "monitor2.png")
    (:name "monitor3" :type :image :file "monitor3.png")
    (:name "monitor4" :type :image :file "monitor4.png"))

(define-block monitor 
  (hp :initform 3) fleeing
  (direction :initform (random-choose '(:up :down)))
  (tags :initform '(:monitor :enemy :target))
  (image :initform "monitor2"))

(defun monitor-scaling-speed ()
  (with-difficulty 1.1 1.2 1.25 1.3 1.35 1.4 1.45 1.5))

(define-method grow monitor ()
  (let ((size (+ %width (monitor-scaling-speed))))
    (when (< size 160)
      (resize self size size)
      (move-toward self :upleft (monitor-scaling-speed)))))

(define-method shrink monitor ()
  (let ((size (- %width (monitor-scaling-speed))))
    (when (> size 42)
      (resize self size size)
      (move-toward self :downright (monitor-scaling-speed)))))

(define-method choose-new-direction monitor ()
  (setf %direction
	(if (= 0 (random 20))
	    ;; occasionally choose a random dir
	    (nth (random 3)
		 '(:up :down :right :left))
	    ;; otherwise turn left
	    (getf '(:up :left :left :down :down :right :right :up)
		  (or %direction :up)))))

(define-method flee monitor ()
  (setf %heading (+ pi (heading-to-cursor self)))
  (forward self 3.2))

(define-method stop-fleeing monitor ()
  (setf %fleeing nil))

(defresource (:name "magenta-alert.wav"
	      :type :sample :file "magenta-alert.wav" 
	      :properties (:volume 15)))

(define-method hunt monitor ()
  (let ((dist (distance-to-cursor self)))
    ;; hunt for player
    (if (< dist (with-difficulty 300 325 350 360 370 380 420))
	(progn 
	  (setf %heading (heading-to-cursor self))
	  (forward self (with-difficulty 1.3 1.4 1.6 1.8 2.0 2.1 2.2 2.3)))
	;; patrol
	(progn (percent-of-time 1 (choose-new-direction self))
	       (move-toward self %direction (with-difficulty 1 2 2 2.5 2.5 3))))))

(defresource "grow.wav" :volume 10)
(defresource "grow2.wav" :volume 10)

(define-method update monitor ()
  (setf %image 
	(ecase %hp
	  (3 "monitor2")
  	  (2 (random-choose '("monitor" "monitor2")))
	  (1 (random-choose '("monitor3" "monitor4")))
	  (0 "monitor")))
  (if (= %hp 1)
      (progn (move self (heading-to-cursor self) (with-difficulty 1.2 1.25 1.3 1.5 1.8 2.0 2.2 2.4))
	     (percent-of-time 25
	       (percent-of-time 30 (play-sound self (random-choose '("grow.wav" "grow2.wav"))))
	       (grow self)))
      (if %fleeing 
	  (flee self)
	  (hunt self))))

(define-method collide monitor (thing)
  (when (not (or (enemyp thing) (holep thing)))
    (restore-location self)
    (when (robotp thing) (die thing))
    ;; (when %fleeing (setf %fleeing nil))
    (choose-new-direction self)))

(define-method damage monitor (points)
  (decf %hp)
  (play-sound self (random-choose *whack-sounds*))
  (when (zerop %hp)
    (multiple-value-bind (x y) (center-point self)
      (let ((size (truncate (/ %width (units 1)))))
	(when (> size 2)
	  (dotimes (n size)
	    (make-sparks x y "magenta"))))
      (make-sparks x y "yellow")
      (play-sound self "xplod.wav")
      (destroy self))))

(define-method fire monitor (heading)
  (multiple-value-bind (x y) (center-point self)
    (drop self (new 'bullet heading :timer 40))))

;;; Ghost 

(defresource 
    (:name "ghost.png" :type :image :file "ghost.png")
    (:name "ghost2.png" :type :image :file "ghost2.png")
    (:name "ghost3.png" :type :image :file "ghost3.png")
    (:name "ghost4.png" :type :image :file "ghost4.png")
    (:name "ghost5.png" :type :image :file "ghost5.png")
    (:name "ghost6.png" :type :image :file "ghost6.png"))

(defparameter *ghost-images* 
 '("ghost.png" "ghost2.png" "ghost3.png" "ghost4.png" "ghost5.png" "ghost6.png"))

(defresource "datanoise.wav" :volume 30)
(defresource "death.wav" :volume 50)
(defresource "death-alien.wav" :volume 50)

(defun ghostp (thing)
  (and (blockyp thing)
       (has-tag thing :ghost)))

(define-block ghost 
  :image "ghost2.png" 
  :hp 5
  :tags '(:ghost :enemy :target)
  :timer 0
  :fleeing nil)

(define-method initialize ghost ()
  (initialize%super self)
  (resize self 140 140))

(define-method damage ghost (points)
  (decf %hp)
  (play-sound self (random-choose '("death.wav" "death-alien.wav")))
  (when (zerop %hp)
    (play-sound self "bigboom.wav")
    (make-sparks %x %y)
    (destroy self)))

(define-method fire ghost (heading)
  (multiple-value-bind (x y) (center-point self)
    (drop self (new 'bullet heading :timer 40))))

(defun ghost-image (hp)
  (case hp
    (5 (or (percent-of-time 2 "ghost.png") "ghost3.png"))
    (4 (or (percent-of-time 4 "ghost.png") "ghost2.png"))
    (3 (or (percent-of-time 30 "ghost2.png") "ghost4.png"))
    (2 (random-choose (list "ghost3.png" (or (percent-of-time 30 "ghost2.png") "ghost4.png"))))
    (1 (random-choose *ghost-images*))
    (0 "ghost.png")))

(define-method update ghost ()
  (change-image self (ghost-image %hp))
  ;; chatter sound
  (percent-of-time 6 (play-sound self "datanoise.wav"))
  (with-fields (timer) self
    (setf timer (max 0 (1- timer))) 
    (let ((dir (heading-to-cursor self))
	  (dist (distance-to-cursor self)))
      (cond 
	;; shoot then set flag to run away
	((and (< dist (with-difficulty 250 250 250 250 300 350 350)) 
	      (zerop timer))
	 ;; don't always fire
	 (percent-of-time (with-difficulty 65 65 65 65 65 70 80 85)  
	   (play-sound self "robovoxx.wav")
	   (fire self dir))
	 (aim self (- dir 0.62))
	 (setf timer 90))
	;; begin approach after staying still
	((and (< dist 570) (zerop timer))
	 (aim self dir)
	 (forward self 2))
	;; run away fast
	((and (< dist 520) (plusp timer))
	 (aim self (- %heading 0.03))
	 (percent-of-time (with-difficulty 1 2 2 2 3 3 4 5)
	   (play-sound self "magenta-alert.wav")
	   (drop self (new 'bullet (heading-to-cursor self)) 14 14))
	 (forward self 3))
	;; otherwise do nothing
	))))

(define-method collide ghost (thing)
  (cond 
    ((or (brickp thing) (enemyp thing))
     (restore-location self)
     (setf %timer 40))
    ((robotp thing)
     (damage thing 1))))

;;; Swarming Shockers

(define-block shocker
  (hp :initform 2)
  (tags :initform '(:shocker :enemy :target))
  (image :initform "shocker.png"))

(define-method update shocker ()
  (when (= %hp 1) (setf %image (random-choose '("shocker.png" "shocker2.png"))))
  (if (< (distance-to-cursor self)
	 (with-difficulty 300 300 300 300 350 400 400 425))
      (move self 
	    (or (percent-of-time 65 (heading-to-cursor self))
		(progn 
		  (percent-of-time 15 (play-sound self (random-choose '("sense.wav" "sense2.wav"))))
		  (random (* 2 pi))))
	    (with-difficulty 2 2 2 3 3 4 4 5 5))
      (progn 
	(move self %heading 1)
	(incf %heading (radian-angle (with-difficulty 2.5 2.5 2.5 2.5 2 1.5))))))

(define-method damage shocker (points)
  (play-sound self (random-choose *whack-sounds*))
  (decf %hp)
  (when (zerop %hp)
    (make-sparks %x %y "hot pink")
    (play-sound self "woom.wav")
    (destroy self)))

(define-method collide shocker (thing)
  (when (robotp thing)
    (die thing))
  (when (brickp thing)
    (restore-location self)
    (setf %heading (opposite-heading %heading))))

;;; Black holes 

(defresource "hole1.png")
(defresource "hole2.png")

(define-block hole 
  (tags :initform '(:hole))
  (clock :initform 40)
  (image :initform "hole1.png"))

(defresource "hole.wav" :volume 20)

(defun hole-clock () (with-difficulty 140 130 130 125 120 120 115 110))

(defun level-beast () 
  (if (<= *level* 5)
      (new 'monitor)
      (new (random-choose '(monitor shocker)))))

(define-method update hole ()
  (when (< (distance-to-cursor self)
	   (with-difficulty 350 400 420 440 460 460))
    (with-fields (clock) self
      (decf clock)
      (when (zerop clock)
	(drop self (new 'monitor))
	(setf clock (hole-clock))))))
  
(define-method collide hole (thing)
  (when (brickp thing)
    (setf %clock (hole-clock))))

;;; Starbase 

(define-block base 
  (tags :initform '(:base :enemy))
  (clock :initform 120)
  (image :initform "resonator.png")
  (hp :initform 8))

(define-method damage base (points))
  ;; (decf %hp)
  ;; (play-sound self (random-choose *whack-sounds*))
  ;; (unless (plusp %hp)
  ;;   (play-sound self "bigboom.wav")
  ;;   (make-sparks %x %y "white")
  ;;   (destroy self)))

(define-method collide base (thing)
  (when (brickp thing)
    (setf %clock 100)))

(defun base-clock () (with-difficulty 120 120 120 110 110 100 95 95 90))

(define-method update base ()
  (when (<= %hp 7)
    (setf %image (random-choose '("resonator.png" "resonator-on.png"))))
  (when (< (distance-to-cursor self)
	   (with-difficulty 350 400 420 440 460 460 480 480))
    (with-fields (clock) self
      (decf clock)
      (when (zerop clock)
	(let ((shocker (new 'shocker)))
	  (play-sound self "woom.wav")
	  (setf (%heading shocker) (random (* 2 pi)))
	  (drop self shocker)
	  (setf clock (base-clock)))))))

;;; Shockwaves

(define-block wave :image "corruption-horz2.png" :tags '(:enemy :wave) :heading (random-choose (list (/ pi 2) (/ pi -2))))

(define-method update wave ()
  (let ((speed
	  (if (> (distance-to-cursor self)
		 (with-difficulty 100 100 100 100 100 150 200 250 275))
	      (with-difficulty 1 1 2 2 3 3)
	      (with-difficulty 2 2 2 2 2 3 3))))
    (percent-of-time 40 (setf %image (random-choose '("corruption-horz2.png" "corruption-horz.png"))))
    (forward self speed)))

(define-method collide wave (thing)
  (when (brickp thing)
    (restore-location self)
    (setf %heading (- %heading pi))))

(define-method damage wave (thing) nil)

  ;;; Deadly burning gas clouds

(defresource 
    (:name "geiger1.wav" :type :sample :file "geiger1.wav" :properties (:volume 10))
    (:name "geiger2.wav" :type :sample :file "geiger2.wav" :properties (:volume 10))
  (:name "geiger3.wav" :type :sample :file "geiger3.wav" :properties (:volume 10))
  (:name "geiger4.wav" :type :sample :file "geiger4.wav" :properties (:volume 10)))

(defparameter *vent-sounds* '("geiger1.wav" "geiger2.wav" "geiger3.wav" "geiger4.wav"))

(defresource 
    (:name "vent.png" :type :image :file "vent.png")
    (:name "vent2.png" :type :image :file "vent2.png")
  (:name "vent3.png" :type :image :file "vent3.png")
  (:name "vent4.png" :type :image :file "vent4.png")
  (:name "vent5.png" :type :image :file "vent5.png"))

(defparameter *vent-images* '("vent.png" "vent2.png" "vent3.png" "vent4.png" "vent5.png"))

(define-block cloud 
  :timer 300
  :collision-type :passive
  :tags '(:cloud)
  :image "vent.png")

(defun cloudp (thing)
  (and (blockyp thing)
       (has-tag thing :cloud)))

(define-method draw cloud ()
  (with-field-values (x y width height image) self
    (let ((jitter (random 10)))
      (when (> jitter 7)
	(incf %heading (random-choose '(-0.3 0.2))))
      (set-blending-mode :additive2)
      (draw-image image
		  (- x jitter)
		  (- y jitter)
		  :width (+ width jitter)
		  :height (+ height jitter)
		  :opacity 1)
      (dotimes (n 4) 
	(draw-box (+ x -5 (random height))
		  (+ y -5 (random width))
		  (+ 5 (random 8))
		  (+ 5 (random 8))
		  :color (random-choose '("white" "magenta"))
		  :alpha 0.7)))))

(define-method initialize cloud (&optional (size (+ 16 (random 32))))
  (initialize%super self)
  (resize self size size))

(define-method update cloud ()
  (forward self 1)
  (decf %timer)
  (when (evenp %timer)
    (setf %image (random-choose *vent-images*)))
  (when (< (distance-to-cursor self) 300)
    (percent-of-time 8 (play-sample (random-choose *vent-sounds*))))
  (unless (plusp %timer)
    (destroy self)))

(defresource 
    (:name "vent-hole1.png" :type :image :file "vent-hole1.png")
    (:name "vent-hole2.png" :type :image :file "vent-hole2.png")
  (:name "vent-hole3.png" :type :image :file "vent-hole3.png"))

(defparameter *vent-hole-images* '("vent-hole1.png" "vent-hole2.png" "vent-hole3.png"))

(define-block vent 
  :image "vent-hole1.png" :tags '(:enemy :vent) 
  :timer 0)

(define-method update vent ()
  (percent-of-time 12 (setf %image (random-choose *vent-hole-images*)))
  (with-fields (timer) self 
    (when (plusp timer)
      (decf timer))
    (when (zerop timer)
      (percent-of-time 3
	(when (< (distance-to-cursor self) 350)
	  (setf timer 20))))
    (percent-of-time 0.6
      (drop self (new 'cloud) 40 40))))

(define-method damage vent (points) nil)

(define-method collide vent (thing)
  (when (robotp thing)
    (die thing)))

;;; Sticky bombs

(defun bombp (thing)
  (has-tag thing :bomb))

(defresource 
    (:name "bomb1.png" :type :image :file "bomb1.png")
    ;; deliberate repeat
    (:name "bomb1.png" :type :image :file "bomb1.png")
  (:name "bomb2.png" :type :image :file "bomb2.png")
  (:name "bomb3.png" :type :image :file "bomb3.png")
  (:name "bomb4.png" :type :image :file "bomb4.png"))

(defparameter *bomb-images* '("bomb1.png" "bomb1.png" "bomb2.png" "bomb3.png" "bomb4.png"))

(defresource
    (:name "bomb-flash1.png" :type :image :file "bomb-flash1.png")
    (:name "bomb-flash2.png" :type :image :file "bomb-flash2.png")
  (:name "bomb-flash3.png" :type :image :file "bomb-flash3.png")
  (:name "explosion.png" :type :image :file "explosion.png")
  (:name "explosion2.png" :type :image :file "explosion2.png"))

(defparameter *explosion-images*
'("bomb-flash1.png" "bomb-flash2.png" "bomb-flash3.png" "explosion.png" "explosion2.png"))

(define-block explosion :tags '(:enemy) :timer (+ 20 (random 12)) :image "explosion.png")

(define-method update explosion ()
  (decf %timer)
  (if (zerop %timer)
      (destroy self)
      (progn
	(setf %image (random-choose *explosion-images*))
	(percent-of-time 4 (play-sample "explode.wav"))
	(resize self (+ 16 (random 16)) (+ 16 (random 16)))
	(move-toward self (random-direction) (+ 6 (random 8))))))

(define-method collide explosion (thing)
  (when (brickp thing) 
    (restore-location self)))

(defun make-explosion (thing &optional (size 8))
  (multiple-value-bind (x y) (center-point thing)
    (dotimes (n size)
      (add-object (current-buffer) (new 'explosion) x y))))

(defresource
    (:name "bomb-ammo.png" :type :image :file "bomb-ammo.png")
    (:name "shield-ammo.png" :type :image :file "shield-ammo.png")
    (:name "energy-ammo.png" :type :image :file "energy-ammo.png")
    (:name "powerup.wav" :type :sample :file "powerup.wav")
    (:name "bombs-away.wav" :type :sample :file "bombs-away.wav" :properties (:volume 70))
    (:name "power.wav" :type :sample :file "power.wav")
    (:name "powerdown.wav" :type :sample :file "powerdown.wav")
    (:name "countdown.wav" :type :sample :file "countdown.wav" :properties (:volume 40))
    (:name "explode.wav" :type :sample :file "explode.wav" :properties (:volume 100)))

(define-block bomb :timer 0 :countdown 5 
  :tags '(:enemy :target :bomb)
  :image "bomb4.png" :target nil
  :stopped nil :speed 4
  :origin nil)

(define-method explode bomb ()
  (make-explosion self)
  (destroy self))

(define-method initialize bomb (heading &key origin)
  (initialize%super self)
;  (setf %image "bomb4.png")
  (setf %origin origin)
  (setf %heading heading))

(define-method collide bomb (thing)
  (cond 
    ;; player ball destroys bomb
    ((ballp thing)
     (explode self))
    ;; bombs should not stick to who fired them
    ((and %origin 
	  (enemyp %origin)
	  (enemyp thing))
     nil)
    ;; enemy bombs stop at player and trail
    ((and %origin 
	  (enemyp %origin)
	  (or (robotp thing)
	      (trailp thing)))
     (setf %stopped t))
    ;; stick to enemies
    ((enemyp thing)
     (setf %target thing))
    ;; stop at walls
    ((brickp thing)
     (setf %stopped t)
     (restore-location self))))

(define-method update bomb () 
  (if %target
      ;; stick to target once touched
      (multiple-value-bind (x y) (center-point %target)
	(move-to self x y))
      ;; move in straight line to find target
      (unless %stopped
	(forward self %speed)))
  ;; possibly explode and/or update timer
  (with-fields (countdown timer image) self
    (if (zerop countdown)
	(explode self)
	(if (plusp timer)
	    (decf timer)
	    (progn
	      (setf timer 20)
	      (play-sample "countdown.wav")
	      (decf countdown)
	      (setf image (or (nth countdown *bomb-images*) "bomb4.png")))))))

;;; A bomber guy who shoots bombs at you

(defresource 
    (:name "blaagh.wav" :type :sample :file "blaagh.wav" :properties (:volume 260))
    (:name "blaagh2.wav" :type :sample :file "blaagh2.wav" :properties (:volume 260))
  (:name "blaagh3.wav" :type :sample :file "blaagh3.wav" :properties (:volume 260))
  (:name "blaagh4.wav" :type :sample :file "blaagh4.wav" :properties (:volume 260)))

(defparameter *rook-sounds* '("blaagh.wav" "blaagh2.wav" "blaagh3.wav" "blaagh4.wav"))

(defresource 
    (:name "alien-1.wav" :type :sample :file "alien-1.wav" :properties (:volume 80))
    (:name "alien-2.wav" :type :sample :file "alien-2.wav" :properties (:volume 80))
  (:name "alien-3.wav" :type :sample :file "alien-3.wav" :properties (:volume 80))
  (:name "alien-4.wav" :type :sample :file "alien-4.wav" :properties (:volume 80))
  (:name "alien-5.wav" :type :sample :file "alien-5.wav" :properties (:volume 80))
  (:name "alien-6.wav" :type :sample :file "alien-6.wav" :properties (:volume 80))
  (:name "alien-7.wav" :type :sample :file "alien-7.wav" :properties (:volume 80)))

(defparameter *alien-sounds*
'("alien-1.wav" "alien-2.wav" "alien-3.wav" "alien-4.wav" "alien-5.wav" "alien-6.wav" "alien-7.wav"))

(defresource 
    (:name "rook.png" :type :image :file "rook.png")
    (:name "rook2.png" :type :image :file "rook2.png")
    (:name "rook3.png" :type :image :file "rook3.png"))

(defun is-rook (thing)
  (has-tag thing :rook))

(define-block rook 
  :image "rook2.png" 
  :hp 12
  :tags '(:rook :enemy :target)
  :timer 0
  :shield-pieces 20
  :fleeing nil)

(define-method deploy-shield rook (pieces)
  (setf %shield-pieces pieces))

(define-method damage rook (points)
  (decf %hp)
  (play-sound self (random-choose *rook-sounds*))
  (when (zerop %hp)
    (make-explosion self 5)
    (play-sound self "xplod.wav")
    (play-sound self "bigboom.wav")
    (destroy self)))

(define-method fire rook (heading)
  (drop self (new 'bomb heading :origin self)))

(define-method update rook ()
  (percent-of-time 20 (setf %image (random-choose '("rook.png" "rook2.png" "rook3.png"))))
  (with-fields (timer) self
    (setf timer (max 0 (1- timer))) 
    (let ((dir (heading-to-cursor self))
	  (dist (distance-to-cursor self)))
      (when (and (< dist 400) (plusp %shield-pieces))
	(drop self (new 'glitch) (/ 2 %width) (/ 2 %height))
	(decf %shield-pieces))
      (cond 
	;; shoot bomb then set flag to run away
	((and (< dist 340) 
	      (zerop timer))
	 ;; don't always fire
	 (percent-of-time 65 
	   (percent-of-time 75 (play-sound self (random-choose *alien-sounds*)))
	   (play-sample "robovoxx.wav")
	   (fire self dir))
	 (aim self (- dir 0.52))
	 (setf timer 90))
	;; begin approach after staying still
	((and (< dist 570) (zerop timer))
	 (aim self dir))
	 ;;(forward self 2))
	;; run away fast
	((and (< dist 420) (plusp timer))
	 (aim self (- %heading 0.03))
	 (percent-of-time (with-difficulty 0 1.0 1.3) 
	   (play-sample "magenta-alert.wav")
	   (drop self (new 'bullet (heading-to-cursor self)) 14 14)))
	 ;;(forward self 3))
	;; otherwise do nothing
	))))

(define-method collide rook (thing)
  (cond 
    ((or (brickp thing) (enemyp thing))
     (restore-location self)
     (setf %timer 40))
    ((robotp thing)
     (damage thing 1))))

;; (defresource "drone.png")

;; (define-block drone :image "drone.png")
