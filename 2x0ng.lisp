;;; 2x0ng.lisp --- the further evolution of dto puzzle games

;; Copyright (C) 2010, 2011, 2012, 2013  David O'Toole

;; Author: David O'Toole <dto@ioforms.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :2x0ng)

(eval-when (:load-toplevel) 
  (setf *window-title* "2x0ng v1.5")
  (setf *default-texture-filter* :nearest)
  (setf *use-antialiased-text* nil)
  (setf *current-directory*
	(make-pathname
	 :directory (pathname-directory #.#P"./"))))

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
    screen))

(define-method update loading-screen ()
  (decf %clock)
  (when (zerop %clock)
    (switch-to-buffer (2x0ng-level %level))))

(defun begin-game (level)   
  (stop-dialogue)
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

(defresource "title.png")

(define-buffer title 
  (quadtree-depth :initform 4)
  (background-image :initform "title.png"))

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

(define-block scroll :image "ending.png")

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
  (let ((buffer (new 'help-screen)))
    (setf (field-value :game buffer) (current-buffer))
    (bind-event buffer '(:h :control) :resume-playing)
    (bind-event buffer '(:escape) :resume-playing)
    (bind-event buffer '(:space) :resume-playing)
    buffer))

;;; Main program

(defun 2x0ng (&optional (level 1))
  (setf *level* level)
  (setf *lives* *initial-lives*)
  (setf *window-title* "2x0ng v1.5")
  (setf *screen-width* 1280)
  (setf *screen-height* 720)
  (setf *nominal-screen-width* 1280)
  (setf *nominal-screen-height* 720)
  ;; zoomout 
;;  (setf *nominal-screen-width* (* 1280 5))
;;  (setf *nominal-screen-height* (* 720 5))
  ;;
  (setf *scale-output-to-window* t) 
  (setf *default-texture-filter* :nearest)
  (setf *use-antialiased-text* nil)

  (setf *frame-rate* 30)
  
  (disable-key-repeat) 
  
  (setf *font* "sans-mono-bold-11") 
  (with-session 
      (load-project "2x0ng" '(:with-database nil))

    ;; (setf *preload-images* t)
    ;; (setf *preload-samples* t)
    (index-pending-resources)
    (preload-resources)

    ;; (rebuild-quadtree '(0 0 100 100) 11)

    (setf *soundtrack* (derange *soundtrack*))
    (switch-to-buffer (new 'title))
    (play-music "rekall.ogg" :loop t)
    (bind-event (current-buffer)  '(:space) :start-playing)
    (start-session)))

(define-buffer 2x0ng
  (bubble :initform nil)
  (default-events 
     :initform
     '(((:r :control) :reset-life)
       ((:q :control) :quit-game)
       ((:h :control) :help)
       ((:m :control) :toggle-music) 
       ((:p :control) :toggle-pause)
       ((:leftbracket) :toggle-red-green-color-blindness)
       ((:j :control) :toggle-joystick)
       ((:n :control) :next-joystick)
       ;;
       ;; ((:x :alt) :command-prompt)
       ;; ((:g :control) :escape)
       ((:f6 :control) :regenerate))))
       ;;
       ;; ((:x :alt) :command-prompt)
       ;; ((:x :control) :edit-cut)
       ;; ((:c :control) :edit-copy)
       ;; ((:v :control) :edit-paste)
       ;; ((:v :control :shift) :paste-here)
       ;; ((:f9) :toggle-minibuffer)
       ;; ((:f12) :transport-toggle-play)
       ;; ((:g :control) :escape)
       ;; ((:d :control) :drop-selection))))

;;; Disable mouse editing

(defun set-buffer-bubble (bubble)
  (setf (field-value :bubble (current-buffer)) bubble))

(define-method handle-point-motion 2x0ng (x y))
(define-method press 2x0ng (x y &optional button))
(define-method release 2x0ng (x y &optional button))
(define-method tap 2x0ng (x y))
(define-method alternate-tap 2x0ng (x y))

(define-method quit-game 2x0ng ()
  (shut-down)
  (quit))

;;; Various toggles

(define-method toggle-red-green-color-blindness 2x0ng ()
  (setf *red-green-color-blindness* 
	(if *red-green-color-blindness* nil t))
  (drop (cursor) 
	(new 'bubble 
	     (if *red-green-color-blindness*
		 "Red/green color blindness support ON. Full effect requires game reset (Control-R)."
		 "Red/green color blindness support OFF. Full effect requires game reset (Control-R)."))))

(defvar *music-toggled* nil)

(define-method toggle-music 2x0ng ()
  (setf *music-toggled* t)
  (if (sdl-mixer:music-playing-p)
      (halt-music)
      (play-music (random-choose *soundtrack*) :loop t)))

(define-method help 2x0ng () 
  (switch-to-buffer (help-buffer)))

(define-method next-joystick 2x0ng ()
  (let ((n (number-of-joysticks))
	(i *joystick-device-number*))
    (reset-joystick (mod (1+ i) n))
    (drop (cursor) 
	  (new 'bubble (format nil "Choosing joystick number ~D" *joystick-device-number*)))))

(define-method regenerate 2x0ng () (reset-level))

(define-method toggle-joystick 2x0ng ()
  (setf *joystick-enabled* (if *joystick-enabled* nil t))
  (drop (cursor) 
	(new 'bubble 
	     (if *joystick-enabled* 
		 "Joystick support on."
		 "Joystick support off."))))
      
(define-method toggle-pause 2x0ng ()
  (when (not %paused)
    (drop (cursor) 
	  (new 'bubble 
	       "Game paused. Press Control-P to resume play.")))
  (transport-toggle-play self)
  (when (not %paused)
    (loop for thing being the hash-keys of %objects do
      (when (bubblep thing) (destroy thing)))))

(define-method reset-game 2x0ng (&optional (level 1))
  (begin-game level))

(define-method reset-life 2x0ng ()
  (if (zerop *lives*)
      (progn 
	(setf *lives* *initial-lives*)
	(begin-game 1))
      (progn
	(decf *lives*)
	(begin-game *level*))))

(defresource "boss-tag.png")
(defresource "boss-tag2.png")

(define-method after-draw-object 2x0ng (thing)
  (when (has-tag thing :boss) 
    (with-fields (x y width) thing
      (draw-image 
       (random-choose '("boss-tag.png" "boss-tag2.png"))
       (+ x (/ width 2) -16) 
       (- y (units 0.3) (image-height "boss-tag.png"))
       :blend :additive 
       :opacity (+ 0.5 (sin (* 0.3 *updates*)))))))

(define-method draw 2x0ng ()
  (buffer%draw self)
  (when (blockyp %bubble) (draw %bubble)))
	

;;; 2x0ng.lisp ends here
