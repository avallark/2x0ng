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
  (setf *current-directory*
	(make-pathname
	 :directory (pathname-directory #.#P"./"))))

(defresource "title.png")

(define-buffer title 
    (background-image :initform "title.png"))

(define-method start-playing title ()
  (sleep 0.2) ;; allow time for human to remove finger from spacebar
  (begin-game *level*))

(defun begin-game (level)   
  (switch-to-buffer (2x0ng-level level)))

(defun reset-level ()
  (begin-game *level*))

(defun next-level ()
  (begin-game (1+ *level*)))

(defun 2x0ng (&optional (level 1))
  (setf *level* level)
  (setf *window-title* "2x0ng")
  
  (setf *screen-width* 1080)
  (setf *screen-height* 720)
  (setf *nominal-screen-width* 1080)
  (setf *nominal-screen-height* 720)
  ;; (setf *nominal-screen-width* (* 1080 4))
  ;; (setf *nominal-screen-height* (* 720 4))
  
  (setf *scale-output-to-window* t) 
  (setf *default-texture-filter* :nearest)
  (setf *use-antialiased-text* nil)
  
  (setf *frame-rate* 30)
  (setf *dt* 33)
  
  (disable-key-repeat) 
  
  (setf *font* "sans-mono-bold-11") 
  (with-session 
      (load-project "2x0ng" '(:with-database nil))
    (setf *soundtrack* (derange *soundtrack*))
    (switch-to-buffer (new 'title))
    (play-music "rekall" :loop t)
    (bind-event (current-buffer)  '(:space) :start-playing)
    (start-session)))

(define-buffer 2x0ng)

(define-method reset-game 2x0ng (&optional (level 1))
  (setf *soundtrack* (derange *soundtrack*))
  (begin-game level))

;;; 2x0ng.lisp ends here
