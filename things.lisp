(in-package :2x0ng)

;; Standard gamebuffer grid measurement

(defparameter *unit* 14)

(defun units (n) (* n *unit*))

;; Level dimensions, in units

(defparameter *level-height* 70)

(defparameter *level-width* 50)

(defparameter *level-screen-height* 35)

(defparameter *level-screen-width* 50)

;; Is it a robot?

(defun robotp (thing)
  (and (blockyp thing)
       (has-tag thing :robot)))

;; Breakable colored bricks

(defparameter *particle-colors* '("red" "blue")) 

(define-block brick 
  :tags '(:brick :colored)
  :collision-type :passive
  :color "red")

(define-method damage brick (points) (destroy self))

(define-method initialize brick (&optional color)
  (initialize%super self)
  (resize self *unit* *unit*)
  (when color (setf %color color)))

(defun slam (thing)
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

(define-method draw brick ()
  (draw-box %x %y %width %height :color %color))

;;; A non-breakable brick

(defparameter *wall-color* "gray")

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

(defun ballp (thing)
  (and (blockyp thing)
       (has-tag thing :ball)))

(defparameter *ball-normal-speed* (units 0.58))

(defparameter *ball-kick-speed* (units 0.85))

(defparameter *ball-deceleration* (units 0.0))

(define-block ball 
  :kicker nil
  :seeking nil
  :height 12 :width 12
  :color "white"
  :speed 0
  :kick-clock 0 :tags '(:ball :colored)
  :direction :right)

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
    ;; move and decelerate
    (if (plusp speed)
	(progn 
	  (if seeking
	      (move self (heading-to-thing self kicker) speed)
	      (move-toward self direction speed))
	  (decf speed *ball-deceleration*))
	(setf %seeking nil))))

(define-method recently-kicked-by ball (cursor)
  (and (plusp %kick-clock) 
       (object-eq cursor %kicker)))
    
(define-method collide ball (thing)
  (cond 
    ;; stop at robot unless it's the guy who just kicked it.
    ;; helps avoid ball getting stuck.
    ((robotp thing)
     (unless (recently-kicked-by self thing)
       (setf %speed 0)))
    ;; bounce off bricks
    ((brickp thing)
     (slam thing)
     (bounce self))))

(defparameter *ball-kicker-collision-delay* 8)

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
