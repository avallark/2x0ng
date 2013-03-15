;;; speech.el --- story frontend to espeak

;; Copyright (C) 2013  David O'Toole

;; Author: David O'Toole <teleflopter>
;; Keywords: 

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

(require 'cl) 

(defvar *speech-program* "espeak")

(defvar *speech-parameters* 
  '(:voice "-v"
    :volume "-a"
    :pitch "-p"
    :markup "-m"
    :speed "-s"
    :punctuation "--punct"
    :file "-w"))

(defvar speech-additional-parameters '("--stdin" "-l" "0" "-m"))

(defun make-speech-parameter (entry)
  (destructuring-bind (name . value) entry
    (let ((option (getf *speech-parameters* name)))
      (when option
	(list option 
	      (if (stringp (first value))
		  (first value)
		  (format "%S" (first value))))))))

(defun pairwise (x)
  (when (consp x)
    (cons (list (first x) (second x))
	  (pairwise (rest (rest x))))))

(defun make-speech-parameters (babel-params)
  (apply #'append 
	 (mapcar #'make-speech-parameter
		 (pairwise babel-params))))

(defun speech-command-string (params)
  (mapconcat 'identity 
	     (append (list *speech-program*)
		     speech-additional-parameters
		     (make-speech-parameters params))
	     " "))

(defun speech-file (params) 
  (getf params :file))

(defun speech-render (text params file)
  (let ((command
	  (speech-command-string 
	   (if (null file) 
	       params
	       (setf params (plist-put params :file file))))))
    (with-temp-buffer (insert text)
      (shell-command-on-region (point-min) (point-max) command))
    (message "COMMAND: %S " command)))

(defun speech-play (file)
  (shell-command (format "play %S" file)))

(defvar *voices* nil)

(defvar *voice* nil)

(setf *voices* nil)
(setf *voice* nil)

(defmacro define-voice (name &rest args)
  `(push ',(cons name args) *voices*))

(defun voice-parameters (name &rest args)
  (rest (assoc name *voices*)))

(defmacro with-voice (voice &rest body)
  `(let ((*voice* (voice-parameters ,voice)))
     ,@body))

(defun say (key text)
  (let ((file (concat 
	       (substring (symbol-name key) 1)
	       ".wav")))
    (speech-render text *voice* file)
    (speech-play file)))

(define-voice :sandy :pitch 9 :speed 83 :voice mb-us2)
(define-voice :navajo :pitch 12 :speed 90 :voice mb-en1)
(define-voice :peach :pitch 10 :speed 90 :voice mb-fr4-en)
(with-voice :navajo (say :oppression "I can't stand all this oppression."))
(with-voice :peach (say :butt "My butt hurts. Like seriously."))
(speech-play :butt)

(provide 'speech)
;;; speech.el ends here


