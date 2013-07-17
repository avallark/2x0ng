(in-package :2x0ng)

(defparameter *initial-lives* 3)

(defparameter *lives* *initial-lives*)

(defparameter *unit* 14)

(defun units (n) (* n *unit*))

(defparameter *level* 0)

(defparameter *difficulty* 0)

(defun with-difficulty (&rest args)
  (if (<= (length args) *difficulty*)
      (nth (1- (length args)) args)
      (nth *difficulty* args)))

(defparameter *depth* 0)

(defun with-depth (&rest args)
  (if (<= (length args) *depth*)
      (nth (1- (length args)) args)
      (nth *depth* args)))

(defparameter *two-brick-themes* 
  '((:snefru "DarkSlateBlue" "green" 
     "magenta" "cyan")
    (:xalcrys "black" "blue violet" 
     "deep pink" "cornflower blue")
    (:zupro "olive drab" "hot pink" 
     "cyan" "yellow")))

(defparameter *three-brick-themes*
  '((:snafu "dark magenta" "gray20" 
     "cyan" "red" "yellow")
    (:atlantis "maroon" "dodger blue" 
     "pale green" "hot pink" "red")
    (:radium "dark olive green" "gold"
     "cyan" "chartreuse" "magenta")
    (:krez "dark orchid" "maroon2" 
     "green" "red" "yellow")))

(defparameter *four-brick-themes*
  '((:zerk "black" "gray40" 
     "maroon2" "yellow green" "orange" "cornflower blue")
    (:tandy "DarkSlateBlue" "gray80" 
     "yellow" "green" "cyan" "deep pink")
    (:zendium "gray17" "orchid"
     "deep pink" "deep sky blue" "chartreuse" "orange")
    (:command "dim gray" "yellow" 
     "cyan" "deep pink" "red" "green yellow")))

(defvar *red-green-color-blindness* nil)

(defparameter *red-green-color-blindness-theme* 
  '("cornflower blue" "yellow" "pale green" "violet red"))

(defun red-green-color-blindness-theme (&optional (colors 4))
  (append (list "black" "gray50")
	  (subseq (derange *red-green-color-blindness-theme*) 
		  0 colors)))

(defparameter *boss-theme* '(:voltz "black" "gray30" "orchid" "medium orchid" "dark orchid" "deep pink" "green yellow"))

(defparameter *themes* 
  (append *two-brick-themes* *three-brick-themes*
	  *four-brick-themes* (list *boss-theme*)))

(defun find-theme (name)
  (rest (assoc name *themes*)))

(defparameter *theme* '("black" "white" "red" "blue"))

(defun theme-colors (&optional (theme *theme*))
  (rest (rest theme)))

(defun color-hash (color)
  (let ((pos (position color (theme-colors) :test 'equal)))
    (when pos (1+ pos))))

(defun set-theme (&optional (theme :wizard))
  (setf *theme* 
	(if (consp theme) 
	    theme
	    (find-theme theme))))

(defun random-theme () (random-choose (mapcar #'car *themes*)))

(defun set-random-theme () (set-theme (random-theme)))

(defun background-color ()
  (when *theme* (first *theme*)))

(defun wall-color ()
  (when *theme* (second *theme*)))

(defun level-colors ()
  (when *theme* (rest (rest *theme*))))

(defparameter *levels* 
  '((:difficulty 0 :colors 2 :hazards nil :wildcards nil)
    ;; 1
    (:difficulty 1 :colors 2 :hazards (hole) :wildcards nil) 
    (:difficulty 1 :colors 2 :hazards (hole paddle) :wildcards nil)
    (:difficulty 2 :colors 3 :hazards (hole hole paddle tracer) :wildcards nil)
    (:difficulty 2 :colors 3 :hazards (hole tracer paddle) :wildcards (ghost thief nil))
    ;; 5
    (:difficulty 2 :colors 3 :hazards (hole hole tracer) :wildcards (biclops))
    (:difficulty 3 :colors 3 :hazards (tracer paddle hole) :wildcards (thief biclops))
    (:difficulty 3 :colors 4 :hazards (hole hole paddle) :wildcards (wave nil))
    (:difficulty 3 :colors 3 :hazards (hole hole paddle) :wildcards (thief ghost))
    (:difficulty 4 :colors 3 :hazards (paddle tracer) :wildcards (rook))
    ;; 10
    (:difficulty 4 :colors 4 :hazards (wave paddle hole) :wildcards nil)
    (:difficulty 4 :colors 3 :hazards (paddle hole hole) :wildcards (ghost thief))
    (:difficulty 5 :colors 3 :hazards (base paddle tracer) :wildcards (ghost wave biclops))
    (:difficulty 5 :colors 4 :hazards (base paddle wave) :wildcards (rook))
    (:difficulty 6 :colors 3 :hazards (base hole wave) :wildcards nil)
    ;; 15
    (:difficulty 6 :colors 4 :hazards (paddle tracer wave) :wildcards (ghost rook))
    (:difficulty 6 :colors 5 
     :music ("xioforms.ogg"))))

(defun nth-level (level)
  (nth (mod level (length *levels*)) *levels*))

(defun level-difficulty (&optional (level *level*))
  (getf (nth-level level) :difficulty))

(defun level-music (&optional (level *level*))
  (or (getf (nth-level level) :music)
      *soundtrack*))

(defun level-theme (&optional (level *level*))
  (let ((ncolors (getf (nth-level level) :colors)))
    (random-choose 
     (mapcar #'car
	     (ecase ncolors
	       (2 *two-brick-themes*)
	       (3 *three-brick-themes*)
	       (4 *four-brick-themes*)
	       (5 '((:voltz))))))))

(defun level-hazards (&optional (level *level*))
  (getf (nth-level level) :hazards))

(defun level-wildcards (&optional (level *level*))
  (getf (nth-level level) :wildcards))

(defun bulkhead ()
  (new 'wall 200 20))

(defun make-hazard ()
  (let ((hazards (level-hazards)))
    (if hazards 
	(let ((hazard (random-choose hazards)))
	  (when hazard 
	    (if (eq 'tracer hazard) 
		(new (random-choose '(paddle paddle paddle paddle tracer)))
		(new hazard))))
	(bulkhead))))

(defun make-wildcard ()
  (let ((wildcards (level-wildcards)))
    (if wildcards 
	(let ((wildcard (random-choose wildcards)))
	  (when wildcard (new wildcard)))
	(bulkhead))))

(defun configure-level (level) 
  (assert (integerp level))
  (setf *level* level)
  (setf *difficulty* (level-difficulty level))
  (let ((ncolors (getf (nth-level level) :colors)))
    (set-theme 
     (if (= 5 ncolors)
	 :voltz
	 (if *red-green-color-blindness*
	     (red-green-color-blindness-theme ncolors) 
	     (level-theme *level*))))))
