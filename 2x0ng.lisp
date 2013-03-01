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

(setf *window-title* "2x0ng")

(setf *screen-width* 920)
(setf *screen-height* 600)
(setf *nominal-screen-width* 1000)
(setf *nominal-screen-height* 750)

(setf *scale-output-to-window* t) 
(setf *default-texture-filter* :nearest)
(setf *use-antialiased-text* nil)

(setf *frame-rate* 30)
(setf *dt* 33)

(disable-key-repeat) 

(setf *font* "sans-mono-bold-11") 

(defun begin-game (level)   
  (when (= 0 level)
    (setf *level-themes* (make-theme-sequence)))
  (switch-to-buffer (2x0ng-level level)))

(defun 2x0ng ()
  (with-session 
      (load-project "2x0ng" '(:with-database nil))
    (setf *soundtrack* (derange *soundtrack*))
    (begin-game 0)
    (start-session)))

(define-buffer 2x0ng)

(define-method reset-game 2x0ng (&optional (level 0))
  (setf *soundtrack* (derange *soundtrack*))
  (begin-game level))

;;; 2x0ng.lisp ends here
