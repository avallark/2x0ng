(in-package :2x0ng)

(defparameter *level* 0)

(defun level-value (&rest args)
  (if (<= (length args) *level*)
      (nth (1- (length args)) args)
      (nth *level* args)))

(defun holding-space ()
  (keyboard-down-p :space))     

(defun targetp (thing)
  (and (blockyp thing)
       (has-tag thing :target)))

(defun enemyp (thing)
  (and (blockyp thing)
       (has-tag thing :enemy)))

(defresource 
    (:name "xplod"
     :type :sample :file "xplod.wav" 
     :properties (:volume 30)))

(defresource 
      (:name "boop1.wav" :type :sample :file "boop1.wav" :properties (:volume 20))
      (:name "boop2.wav" :type :sample :file "boop2.wav" :properties (:volume 20))
    (:name "boop3.wav" :type :sample :file "boop3.wav" :properties (:volume 20)))

(defparameter *bounce-sounds* '("boop1.wav" "boop2.wav" "boop3.wav"))

(defparameter *doorbell-sounds*
  (defresource 
      (:name "doorbell.wav" :type :sample :file "doorbell.wav" :properties (:volume 20))
      (:name "doorbell2.wav" :type :sample :file "doorbell2.wav" :properties (:volume 20))
    (:name "doorbell3.wav" :type :sample :file "doorbell3.wav" :properties (:volume 20))))

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
    (:name "color1.wav" :type :sample :file "color1.wav" :properties (:volume 12))
    (:name "color2.wav" :type :sample :file "color2.wav" :properties (:volume 12))
  (:name "color3.wav" :type :sample :file "color3.wav" :properties (:volume 12)))

(defparameter *color-sounds* '("color1.wav" "color2.wav" "color3.wav"))

(defparameter *bonux-sounds*
  (defresource 
      (:name "bonux1.wav" :type :sample :file "bonux1.wav" :properties (:volume 12))
      (:name "bonux2.wav" :type :sample :file "bonux2.wav" :properties (:volume 12))
    (:name "bonux3.wav" :type :sample :file "bonux3.wav" :properties (:volume 12))))

;;; Sparkle explosion cloud fx

(define-block spark 
  :width 3 :height 3 :color nil)
;  :collision-type nil)

(define-method collide spark (thing)
  (when (enemyp thing) 
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
  :timer 60
  :blend :alpha
  :growth-rate nil
  :tags '(:bullet))

(define-method draw bullet ()
  (draw-circle %x %y %radius 
	       :color (random-choose 
		       (if (player-bullet-p self)
			   '("green" "yellow")
			   '("cyan" "white" "deep sky blue")))
	       :type :solid))
	      
(define-method update bullet ()
  (decf %timer)
  (if (zerop %timer) 
      (destroy self)
      (forward self %speed)))

(define-method collide bullet (thing)
  (cond 
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

;; Color themes

(defparameter *two-brick-themes* 
  '((:snefru "saddle brown" "green" 
     "orange" "cyan")
    (:xalcrys "black" "cornflower blue" 
     "yellow" "red")
    (:zupro "gray30" "red" 
     "blue" "cornflower blue")))

(defparameter *three-brick-themes*
  '((:snafu "dark magenta" "gray20" 
     "cyan" "red" "yellow")
    (:atlantis "blue" "yellow" 
     "white" "green" "yellow")
    (:krez "black" "maroon2" 
     "green" "yellow" "orange")))

(defparameter *four-brick-themes*
  '((:zerk "black" "gray40" 
     "maroon2" "green" "yellow" "orange")
    (:tandy "DarkSlateBlue" "gray80" 
     "blue violet" "orchid" "cyan" "deep pink")
    (:command "red" "cyan" 
     "black" "magenta" "hot pink" "orange")))

(defparameter *themes* (append *two-brick-themes* *three-brick-themes* *four-brick-themes*))

    ;; (:vcs "black" "red" "goldenrod" "khaki" "cornsilk")
    ;; (:tandy "MidnightBlue" "gray80" "yellow" "orchid" "purple")
    ;; (:vax "gray12" "orange" "cyan" "deep pink" "orchid")
    ;; (:surround "gray30" "goldenrod" "yellow" "yellow green" "red")
    ;; (:wizard "midnight blue" "slate blue" "dark orange" "orange" "gold")))

(defun find-theme (name)
  (rest (assoc name *themes*)))

(defparameter *theme* (find-theme :wizard))

(defun theme-colors (&optional (theme *theme*))
  (rest (rest theme)))

(defun set-theme (&optional (theme :wizard))
  (setf *theme* (find-theme theme)))

(defun random-theme () (random-choose (mapcar #'car *themes*)))

(defun set-random-theme () (set-theme (random-theme)))

(defun background-color ()
  (when *theme* (first *theme*)))

(defun wall-color ()
  (when *theme* (second *theme*)))

(defun brick-colors ()
  (when *theme* (rest (rest *theme*))))

;; Standard gamebuffer grid measurement

(defparameter *unit* 14)

(defun units (n) (* n *unit*))

;; Level dimensions, in units

(defparameter *level-height* 200)

(defparameter *level-width* 200)

(defparameter *level-screen-height* 35)

(defparameter *level-screen-width* 50)

;; Is it a robot?

(defun robotp (thing)
  (and (blockyp thing)
       (has-tag thing :robot)))

;; Breakable colored bricks

(define-block brick 
  :tags '(:brick :colored)
  ;;  :collision-type :passive
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
  (set-blending-mode :alpha)
  (draw-box %x %y %width %height :color %color))

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
    (when (not (string= %color color))
      (play-sample (random-choose *color-sounds*)))
    (setf %color color)))

(defun same-color (a b)
  (string= (color-of a) (color-of b)))

;;; A non-breakable brick

(defparameter *wall-color* "gray50")

(define-block (wall :super brick)
  (tags :initform '(:brick :wall))
  (color :initform  *wall-color*))

(defun wallp (thing)
  (and (blockyp thing)
       (has-tag thing :wall)))

(define-method damage wall (points) 
  ;; TODO flash and/or make a cool (themeable) sound 
  nil)

;;; A colored block that only opens with the right color

(defresource "gate.png")
(defresource "gate-closing-sound.wav" :volume 150)

(defresource "error.wav" :volume 70)

(defun gatep (thing)
  (and (blockyp thing)
       (has-tag thing :gate)))

(define-block (gate :super brick)
  (tags :initform '(:brick :colored :gate))
  (color :initform "white"))

(define-method draw gate ()
  (draw-textured-rectangle %x %y %z %width %height 
			   (find-texture "gate.png")
			   :vertex-color %color))

(define-method paint gate (color)
  (if (not (string= %color color))
      (play-sample "error.wav")
      (progn 
	(play-sample "gate-closing-sound.wav")
	(destroy self))))

(define-method damage gate (points))

;;; The football

(defvar *ball* nil)

(defvar *auto-return-distance* 640)

(defparameter *ball-size* (truncate (units 1.2)))

(defun ballp (thing)
  (and (blockyp thing)
       (has-tag thing :ball)))

(defparameter *ball-normal-speed* (units 0.88))

(defparameter *ball-kick-speed* (units 1.2))

(defparameter *ball-deceleration* (units 0.0))

(define-block ball 
  :kicker nil
  :target nil
  :seeking nil
  :height *ball-size* :width *ball-size*
  :color "white"
  :speed 0
  :hits 6
  :kick-clock 0 :tags '(:ball :colored))

(define-method paint ball (color)
  (setf %color color))

(define-method initialize ball (&optional color)
  (initialize%super self)
  (setf *ball* self)
  (resize self *ball-size* *ball-size*)
  (when color (setf %color color)))

(defun make-ball (&optional (color "white"))
  (setf *ball* (new 'ball color)))

(define-method after-deserialize ball ()
  (after-deserialize%super self)
  (setf *ball* self))

(defresource "bounce.wav" :volume 10)

(define-method bounce ball ()
  (play-sound self (random-choose *bounce-sounds*))
  (restore-location self)
  (setf %heading 
	(if %seeking
	    (opposite-heading %heading)
	    (opposite-heading %heading)))
  (move self %heading 2.2)
  (setf %kick-clock 0)
  (setf %seeking (if %seeking nil t)))

(define-method find-enemy ball (thing2 &optional (range 180))
  (let ((enemies
	  (loop for thing being the hash-values of (%objects (current-buffer))
		when (and 
		      (blockyp thing)
		      (enemyp thing)
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
  (when (plusp %kick-clock)
    (decf %kick-clock))
  (with-fields (seeking heading speed kicker) self
    (when (> (distance-between self kicker) *auto-return-distance*)
      (setf %seeking t))
    (if (plusp speed)
	(if %target 
	    (move self (heading-to-thing self %target) (+ speed 2))
	    (if seeking
		(move self (heading-to-thing self kicker) speed)
		(move self heading speed)))
	(setf %seeking nil))))

(define-method recently-kicked-by ball (cursor)
  (and (plusp %kick-clock) 
       (object-eq cursor %kicker)))
    
(define-method collide ball (thing)
  (cond 
    ((gatep thing)
     (bounce self)
     (when (same-color self thing)
       (destroy thing)))
    ((and (enemyp thing) (not (trailp thing)))
     (bounce self)
     (decf %hits)
     (when (has-method :damage thing)
       (damage thing 1))
     (setf %target
	   (when (plusp %hits)
	     (find-enemy self thing)))
     (when %target (setf %seeking nil)))
    ;; stop at robot unless it's the guy who just kicked it.
    ;; helps avoid ball getting stuck.
    ((robotp thing)
     (unless (recently-kicked-by self thing)
       ;; possibly catch ball
       (paint thing (color-of *ball*))
       (destroy self)
       (setf *ball* nil)))
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
    (draw-box (+ 2 x) (+ 2 y) (- width 4) (- height 4) :color color)))

;;; Level exit

(defresource "exit1.png")
(defresource "exit2.png")

(define-block exit
  (image :initform "exit1.png"))

