(in-package :2x0ng)

(defparameter *unit* 14)

(defun units (n) (* n *unit*))

(defparameter *level* 0)

(defparameter *difficulty* 0)

(defun with-difficulty (&rest args)
  (if (<= (length args) *difficulty*)
      (nth (1- (length args)) args)
      (nth *difficulty* args)))

(defparameter *two-brick-themes* 
  '((:snefru "DarkSlateBlue" "green" 
     "magenta" "cyan")
    (:xalcrys "black" "blue violet" 
     "deep pink" "orange")
    (:zupro "olive drab" "hot pink" 
     "cyan" "yellow")))

(defparameter *three-brick-themes*
  '((:snafu "dark magenta" "gray20" 
     "cyan" "red" "yellow")
    (:atlantis "midnight blue" "purple" 
     "green" "hot pink" "cyan")
    (:radium "dark orange" "gold"
     "cyan" "chartreuse" "magenta")
    (:krez "black" "maroon2" 
     "green" "yellow" "orange")))

(defparameter *four-brick-themes*
  '((:zerk "black" "gray40" 
     "maroon2" "green" "yellow" "orange")
    (:tandy "DarkSlateBlue" "gray80" 
     "yellow" "green" "cyan" "deep pink")
    (:zendium "gray17" "orchid"
     "deep pink" "chartreuse" "cyan" "peach puff")
    (:command "black" "goldenrod" 
     "cyan" "hot pink" "red" "orange")))

(defparameter *boss-theme* '(:voltz "black" "black" "red" "orchid" "aquamarine"))

(defparameter *themes* 
  (append *two-brick-themes* *three-brick-themes*
	  *four-brick-themes* (list *boss-theme*)))

(defun find-theme (name)
  (rest (assoc name *themes*)))

(defparameter *theme* '("black" "white" "red" "blue"))

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

(defun level-colors ()
  (when *theme* (rest (rest *theme*))))

(defparameter *levels* 
  '((:difficulty 0 :colors 2 :hazards nil :wildcard nil)
    (:difficulty 0 :colors 2 :hazards (hole) :wildcard nil)
    (:difficulty 1 :colors 2 :hazards (hole paddle) :wildcard nil)
    (:difficulty 1 :colors 3 :hazards (hole paddle) :wildcard nil)
    (:difficulty 2 :colors 2 :hazards (hole paddle) :wildcard ghost)
    (:difficulty 2 :colors 3 :hazards (hole tracer) :wildcard nil)
    (:difficulty 3 :colors 3 :hazards (tracer paddle) :wildcard nil)
    (:difficulty 3 :colors 4 :hazards (hole paddle) :wildcard nil)
    (:difficulty 4 :colors 3 :hazards (hole paddle) :wildcard thief)
    (:difficulty 4 :colors 4 :hazards (hole paddle tracer) :wildcard nil)
    (:difficulty 5 :colors 3 :hazards (glitch paddle) :wildcard nil)
    (:difficulty 5 :colors 4 :hazards (glitch paddle hole) :wildcard nil)
    (:difficulty 6 :colors 3 :hazards (base paddle wave) :wildcard nil)
    (:difficulty 6 :colors 4 :hazards (base paddle tracer) :wildcard nil)
    (:difficulty 7 :colors 3 :hazards (base hole paddle) :wildcard thief)
    (:difficulty 7 :colors 4 :hazards (paddle tracer glitch) :wildcard ghost)
    (:difficulty 8 :colors 3 :hazards (hole paddle base wave) :wildcard nil)
    (:difficulty 8 :colors 4 :hazards (base) :wildcard boss)))

(defun level-difficulty (&optional (level *level*))
  (getf (nth level *levels*) :difficulty))

(defun level-theme (&optional (level *level*))
  (random-choose 
   (mapcar #'car
	   (ecase (getf (nth level *levels*) :colors)
	     (2 *two-brick-themes*)
	     (3 *three-brick-themes*)
	     (4 *four-brick-themes*)))))

(defun level-hazards (&optional (level *level*))
  (getf (nth level *levels*) :hazards))

(defun level-wildcard (&optional (level *level*))
  (getf (nth level *levels*) :wildcard))

(defun make-hazard ()
  (let ((hazard (level-hazards)))
    (if hazard (new hazard) (new 'wall))))

(defun make-wildcard ()
  (let ((wildcard (level-wildcard)))
    (if wildcard (new wildcard) (new 'wall))))

(defun configure-level (level) 
  (assert (integerp level))
  (setf *level* level)
  (setf *difficulty* (level-difficulty level))
  (set-theme (level-theme *level*)))

