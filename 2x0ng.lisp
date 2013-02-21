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

(setf *screen-height* (* *unit* *level-screen-height*))
(setf *screen-width* (* *unit* *level-screen-width*))
;; (setf *nominal-screen-width* 980)
;; (setf *nominal-screen-height* 600)

(setf *scale-output-to-window* nil) 
(setf *default-texture-filter* :nearest)
(setf *use-antialiased-text* nil)

(setf *frame-rate* 30)
(setf *dt* 33)

(disable-key-repeat) 

(setf *font* "sans-mono-bold-16") 

(defun begin-game ()
  (with-buffer (2x0ng-level)
    (start (current-buffer))))

(defun 2x0ng ()
  (with-session 
      (load-project "2x0ng") 
    (begin-game)
    (start-session)))

(define-buffer 2x0ng)

(define-method reset 2x0ng ()
  (begin-game))

;;; 2x0ng.lisp ends here
