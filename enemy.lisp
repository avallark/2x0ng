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
  (collision-type :initform :passive)
  (height :initform 5)
  (width :initform 5))

(define-method draw trail ()
  (draw-box %x %y %width %height :color %color))

(define-method damage trail (points) (destroy self))

(define-method initialize trail ()
  (later 320 (destroy self)))

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
    (percent-of-time 0.5 (drop self (new 'glitch)))
    (drop self (new 'trail) 6 6)
    (setf %energy 4)))

(define-method update tracer ()
  (percent-of-time 0.2 (setf %direction (random-direction)))
  (move-toward self %direction %speed)
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
		 (level-value 200 250 300 350 400))
	      (level-value 8 10 12 14 16) 
	      (level-value 3 4 5 6 7 9 11 13))))
    (forward self speed)))

(define-method collide paddle (thing)
  (when (brickp thing)
    (restore-location self)
    (setf %heading (- %heading pi))))

(define-method damage paddle (thing) nil)

;;; Radioactive corruption glitches that creep after you

(defun is-glitch (thing)
  (has-tag thing :glitch))

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
  (image :initform (random-choose *corruption-images*))
  (speed :initform 1)
  (overlay-color :initform nil))

(define-method damage glitch (points)
  (make-sparks %x %y "yellow green")
  (play-sound self (random-choose *corruption-sounds*))
  (destroy self))

(define-method collide glitch (thing)
  (when (brickp thing)
    (percent-of-time 90 (restore-location self)))
  ;; (when (ballp thing)
  ;;   (play-sound self "blurp.wav")
  ;;   (destroy self))
  (when (robotp thing)
    (die thing)))

(define-method set-overlay glitch ()
  (setf %overlay-color (random-choose '("cyan" "magenta" "yellow" "orange"))))

(define-method clear-overlay glitch ()
  (setf %overlay-color nil))

(define-method creep glitch ()
  (move self (heading-to-cursor self) %speed)
  (when (< (distance-to-cursor self) 460)
    (percent-of-time 2 
      (setf %speed (min 2 (+ %speed 0.3))))))

(define-method update glitch ()
  (percent-of-time 3 (change-image self (random-choose *corruption-images*)))
  (creep self)
  (percent-of-time 3 
    (set-overlay self)
    (later 20 (clear-overlay self))))

(define-method draw glitch ()
  (draw%super self)
  (set-blending-mode :additive2)
  (when %overlay-color
    (draw-box %x %y %width %height
     :alpha 0.2
     :color %overlay-color)))

;;; The "monitor", a roving enemy that fires a spread of bullets then dashes away

(defresource
    (:name "monitor" :type :image :file "monitor.png")
    (:name "monitor2" :type :image :file "monitor2.png")
    (:name "monitor3" :type :image :file "monitor3.png")
    (:name "monitor4" :type :image :file "monitor4.png"))

(define-block monitor 
  (hp :initform 3) fleeing
  (direction :initform (random-choose '(:up :down)))
  (tags :initform '(:monitor :enemy))
  (image :initform "monitor2"))

(defun monitor-scaling-speed ()
  (level-value 1.2 1.3 1.4 1.5 1.6))

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
    (if (< dist (level-value 240 275 300 325 360 400 440 500))
	(progn 
	  (setf %heading (heading-to-cursor self))
	  (forward self (level-value 1.2 1.5 1.5 1.5 1.7 2.0 2.4)))
	;; patrol
	(progn (percent-of-time 1 (choose-new-direction self))
	       (move-toward self %direction (level-value 1 2 2.5 3))))))

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
      (progn (move self (heading-to-cursor self) (level-value 1.3 1.6 2.0 2.2 2.5 2.8 3.0 3.3))
	     (percent-of-time 25
	       (percent-of-time 30 (play-sound self (random-choose '("grow.wav" "grow2.wav"))))
	       (grow self)))
      (if %fleeing 
	  (flee self)
	  (hunt self))))

(define-method collide monitor (thing)
  (when (not (or (enemyp thing) (holep thing)))
    (restore-location self)
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
  :tags '(:ghost :enemy)
  :timer 0
  :fleeing nil)

(define-method initialize ghost ()
  (initialize%super self)
  (resize self 100 100))

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
  (percent-of-time (- 40 (* 5 %hp)) (play-sound self "datanoise.wav"))
  (with-fields (timer) self
    (setf timer (max 0 (1- timer))) 
    (let ((dir (heading-to-cursor self))
	  (dist (distance-to-cursor self)))
      (cond 
	;; shoot then set flag to run away
	((and (< dist (level-value 250 250 250 250 300 350 350)) 
	      (zerop timer))
	 ;; don't always fire
	 (percent-of-time (level-value 65 65 65 65 65 70 80 85)  
	   (play-sound self "robovoxx.wav")
	   (fire self dir))
	 (aim self (- dir 0.62))
	 (setf timer 90))
	;; begin approach after staying still
	((and (< dist 570) (zerop timer))
	 (aim self dir)
	 (forward self 2))
	;; run away fast
	((and (< dist 420) (plusp timer))
	 (aim self (- %heading 0.03))
	 (percent-of-time (level-value 1 1 1 1 2 3 4 5)
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

;;; Black holes 

(defresource "hole1.png")
(defresource "hole2.png")

(define-block hole 
  (tags :initform '(:hole))
  (clock :initform 120)
  (image :initform "hole1.png"))

(defresource "hole.wav" :volume 20)

(defun hole-clock () (level-value 140 130 130 120 110 100 100 95 90))

(define-method update hole ()
  (when (< (distance-to-cursor self)
	   (level-value 350 400 420 440 460 460 480 480))
    (with-fields (clock) self
      (decf clock)
      (when (zerop clock)
	(drop self (new 'monitor))
	(setf clock (hole-clock))))))
  
(define-method collide hole (thing)
  (when (brickp thing)
    (setf %clock (hole-clock))))
