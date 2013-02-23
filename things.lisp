(in-package :2x0ng)

;; Color themes

(defparameter *themes* 
  '((:dec "DarkSlateBlue" "orchid" "cyan" "magenta" "yellow")
    (:zerk "black" "maroon2" "green" "yellow" "orange")
    (:vcs "black" "red" "goldenrod" "khaki" "cornsilk")
    (:tandy "MidnightBlue" "gray80" "yellow" "orchid" "purple")
    (:vax "gray12" "orange" "cyan" "deep pink" "orchid")
    (:command "black" "DarkGoldenrod" "red" "magenta" "cyan")
    (:surround "gray30" "goldenrod" "yellow" "yellow green" "red")
    (:wizard "midnight blue" "slate blue" "dark orange" "orange" "gold")))

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

(defparameter *level-height* 100)

(defparameter *level-width* 100)

(defparameter *level-screen-height* 35)

(defparameter *level-screen-width* 50)

;; Is it a robot?

(defun robotp (thing)
  (and (blockyp thing)
       (has-tag thing :robot)))

;; Breakable colored bricks

(define-block brick 
  :tags '(:brick :colored)
  :collision-type :passive
  :color "gray50")

(define-method damage brick (points) (destroy self))

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

(defun paint (thing color)
  (when (coloredp thing)
    (setf (%color thing) color)))

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

;;; The football

(defvar *ball* nil)

(defparameter *ball-size* (truncate (units 0.8)))

(defun ballp (thing)
  (and (blockyp thing)
       (has-tag thing :ball)))

(defparameter *ball-normal-speed* (units 0.88))

(defparameter *ball-kick-speed* (units 0.85))

(defparameter *ball-deceleration* (units 0.0))

(define-block ball 
  :kicker nil
  :seeking nil
  :height *ball-size* :width *ball-size*
  :color "white"
  :speed 0
  :kick-clock 0 :tags '(:ball :colored)
  :direction :right)

(define-method initialize ball (&optional color)
  (initialize%super self)
  (resize self *ball-size* *ball-size*)
  (when color (setf %color color)))

(defun make-ball (&optional (color "white"))
  (setf *ball* (new 'ball color)))

(defresource "bounce.wav" :volume 10)

(define-method bounce ball ()
  (play-sound self "bounce.wav")
  (restore-location self)
  (setf %direction (opposite-direction %direction))
  (move-toward self %direction 1.5)
  (setf %kick-clock 0)
  (setf %seeking (if %seeking nil t))) 

(define-method update ball ()
  (when (plusp %kick-clock)
    (decf %kick-clock))
  (with-fields (seeking direction speed kicker) self
    ;; move 
    (if (plusp speed)
	(progn 
	  (if seeking
	      (move self (heading-to-thing self kicker) speed)
	      (move-toward self direction speed)))
	  ;(decf speed *ball-deceleration*))
	(setf %seeking nil))))

(define-method recently-kicked-by ball (cursor)
  (and (plusp %kick-clock) 
       (object-eq cursor %kicker)))
    
(define-method collide ball (thing)
  (cond 
    ;; stop at robot unless it's the guy who just kicked it.
    ;; helps avoid ball getting stuck.
    ((robotp thing)
     ;; possibly catch ball
     (paint thing (color-of *ball*))
     (destroy *ball*)
     (setf *ball* nil))
    ;; bounce off bricks
    ((brickp thing)
     (when (coloredp thing)
       (paint self (color-of thing)))
     (slap thing)
     (bounce self))))

(defparameter *ball-kicker-collision-delay* 4)

(define-method impel ball (direction strong &optional kicker)
  (assert (keywordp direction))
  (setf %kicker kicker)
  (setf %kick-clock *ball-kicker-collision-delay*)
  (setf %seeking nil)
  (setf %speed (if strong *ball-kick-speed* *ball-normal-speed*))
  (setf %direction direction))

(define-method draw ball ()
  (with-field-values (x y width height color) self
    (draw-box x y width height :color color)))

;;; Black holes 

(defresource "hole1.png")
(defresource "hole2.png")

(define-block hole 
  (image :initform "hole1.png"))

(defresource "hole.wav" :volume 20)

(define-method collide hole (thing)
  (slap thing)
  (play-sample "hole.wav")
  (move-to self 
	   (- %x (* %width 0.3))
	   (- %y (* %height 0.3)))
  (resize self (* %width 1.3) (* %height 1.3)))
  
