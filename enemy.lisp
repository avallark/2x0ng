(in-package :2x0ng)

;;; Positronic death filament

(defun is-trail (thing)
  (has-tag thing :trail))

(define-block trail
  (tags :initform '(:trail))
  (color :initform (random-choose '("cyan" "orchid" "magenta" "yellow")))
  (collision-type :initform :passive)
  (height :initform 5)
  (width :initform 5))

(define-method draw trail ()
  (draw-box %x %y %width %height :color %color))

(define-method initialize trail ()
  (later 320 (destroy self)))

(define-method collide trail (thing)
  (when (robotp thing)
    (destroy thing)))

;; Nasty tracers 

(defresource "tracer.png")

(define-block tracer
  (speed :initform 1.5)
  (image :initform "tracer.png")
  (direction :initform (random-direction))
  (energy :initform 0))

(define-method lay-trail tracer ()
  (decf %energy)
  (unless (plusp %energy)
    (drop self (new 'trail) 6 6)
    (setf %energy 4)))

(define-method update tracer ()
  (move-toward self %direction %speed)
  (lay-trail self))

(define-method collide tracer (thing)
  (when (brickp thing)
    (restore-location self)
    (setf %energy 6)
    (setf %direction (random-direction))))

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
(defresource (:name "bigboom.wav" :type :sample :file "bigboom.wav" :properties (:volume 60)))

(define-block glitch
  (tags :initform '(:enemy :glitch))
  (image :initform (random-choose *corruption-images*))
  (speed :initform 1)
  (overlay-color :initform nil))

(define-method damage glitch (points)
  (play-sound self (random-choose *corruption-sounds*))
  (destroy self))

(define-method collide glitch (thing)
  (when (brickp thing)
    (percent-of-time 90 (restore-location self)))
  (when (ballp thing)
    (paint thing "white")
    (play-sample "blurp.wav")
    (destroy self))
  (when (robotp thing)
    ;; (damage thing 4)
    (destroy self)))

(define-method set-overlay glitch ()
  (setf %overlay-color (random-choose '("cyan" "magenta" "yellow" "orange"))))

(define-method clear-overlay glitch ()
  (setf %overlay-color nil))

(define-method creep glitch ()
  (move self (heading-to-cursor self) %speed)
  (when (< (distance-to-cursor self) 460)
    (percent-of-time 2 
;      (play-sound self "munch1.wav")
      (let ((size (* %height 1.3)))
	(resize self size size))
      (incf %speed 0.3))))

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
