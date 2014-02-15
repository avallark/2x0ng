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
  (setf *window-title* "2x0ng v1.8")
  (setf *default-texture-filter* :nearest)
  (setf *use-antialiased-text* nil)
  (setf *current-directory*
	(make-pathname
	 :directory (pathname-directory #.#P"./"))))

;;; Main program

(defun 2x0ng (&optional (level 1))
  (setf *level* level)
  (setf *retries* *initial-retries*)
  (setf *window-title* "2x0ng v1.8")
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
  
  (with-session 
      (load-project "2x0ng" '(:with-database nil))

    ;; (setf *preload-images* t)
    ;; (setf *preload-samples* t)
    (index-pending-resources)
    (preload-resources)

    (setf *soundtrack* (derange *soundtrack*))
    (switch-to-buffer (new 'title))
    (play-music "rekall.ogg" :loop t)
    (bind-event (current-buffer)  '(:space) 'start-playing)
    (start-session)))

(define-buffer 2x0ng
  (layered :initform nil)
  (bubble :initform nil)
  (retrying :initform nil)
  (default-events 
     :initform
     '(((:r :control) :reset-game)
       ((:q :control) :quit-game)
       ((:y :control) :show-waypoint)
       ((:h :control) :help)
       ((:m :control) :toggle-music) 
       ((:p :control) :toggle-pause)
       ((:leftbracket) :toggle-red-green-color-blindness)
       ((:j :control) :toggle-joystick)
       ;;       ((:f8) :cheat)
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

(define-method cheat 2x0ng ()
  (room)
  (next-level))

;;; Disable mouse editing

(defun set-buffer-bubble (bubble)
  (setf (field-value :bubble (current-buffer)) bubble))

(define-method handle-point-motion 2x0ng (x y))
(define-method press 2x0ng (x y &optional button))
(define-method release 2x0ng (x y &optional button))
(define-method tap 2x0ng (x y))
(define-method alternate-tap 2x0ng (x y))

(define-method quit-game 2x0ng ()
  (at-next-update (xelf:quit t)))

;;; Various toggles

(define-method toggle-red-green-color-blindness 2x0ng ()
  (setf *red-green-color-blindness* 
	(if *red-green-color-blindness* nil t))
  (drop (cursor) 
	(new 'bubble :text
	     "Red/green color blindness support ON. Full effect requires game reset (Control-R)."
	     "Red/green color blindness support OFF. Full effect requires game reset (Control-R).")))

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
	  (new 'bubble :text (format nil "Choosing joystick number ~D" *joystick-device-number*)))))

(define-method regenerate 2x0ng () (reset-level))

(define-method toggle-joystick 2x0ng ()
  (setf *joystick-enabled* (if *joystick-enabled* nil t))
  (drop (cursor) 
	(new 'bubble :text
	     (if *joystick-enabled* "Joystick support on." "Joystick support off."))))
      
(define-method toggle-pause 2x0ng ()
  (when (not %paused)
    (drop (cursor) 
	  (new 'bubble :text
	       "Game paused. Press Control-P to resume play.")))
  (transport-toggle-play self)
  (when (not %paused)
    (loop for thing being the hash-keys of %objects do
      (when (bubblep thing) (destroy (find-object thing))))))

(define-method reset-game 2x0ng (&optional (level 1))
  (setf *retries* *initial-retries*)
  (begin-game level))

(define-method update 2x0ng ()
  (call-next-method)
  (when %retrying (begin-game *level*)))

(defresource "boss-tag.png")
(defresource "boss-tag2.png")

(define-method after-draw-object 2x0ng (thing)
  (when (has-tag (find-object thing) :boss)
    (with-fields (x y width) thing
      (draw-image 
       (random-choose '("boss-tag.png" "boss-tag2.png"))
       (+ x (/ width 2) -16) 
       (- y (units 0.3) (image-height "boss-tag.png"))
       :blend :additive 
       :opacity (+ 0.5 (sin (* 0.3 *updates*)))))))

(define-method show-waypoint 2x0ng ()
  (drop-waypoint-maybe (cursor) :force))

(define-method draw 2x0ng ()
  (call-next-method)
  (when (xelfp %bubble) (draw (find-object %bubble))))

;;; 2x0ng.lisp ends here
