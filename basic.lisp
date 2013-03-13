(in-package :2x0ng)

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
     "deep pink" "orange")
    (:zupro "olive drab" "hot pink" 
     "cyan" "yellow")))

(defparameter *three-brick-themes*
  '((:snafu "dark magenta" "gray20" 
     "cyan" "red" "yellow")
    (:atlantis "midnight blue" "purple" 
     "green" "hot pink" "cyan")
    (:radium "dark olive green" "gold"
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
  '((:difficulty 0 :colors 2 :hazards nil :wildcards nil)
    (:difficulty 0 :colors 2 :hazards (hole) :wildcards nil) 
    (:difficulty 1 :colors 2 :hazards (hole paddle) :wildcards nil)
    (:difficulty 1 :colors 3 :hazards (hole hole paddle) :wildcards nil)
    (:difficulty 2 :colors 2 :hazards (hole hole paddle) :wildcards (ghost thief))
    (:difficulty 2 :colors 3 :hazards (hole hole tracer) :wildcards nil)
    (:difficulty 3 :colors 3 :hazards (tracer paddle hole) :wildcards nil)
    (:difficulty 3 :colors 4 :hazards (hole hole paddle) :wildcards nil)
    (:difficulty 4 :colors 3 :hazards (hole hole paddle) :wildcards (thief ghost))
    (:difficulty 4 :colors 4 :hazards (hole hole paddle tracer) :wildcards nil)
    (:difficulty 5 :colors 3 :hazards (glitch paddle hole) :wildcards nil)
    (:difficulty 5 :colors 4 :hazards (glitch paddle hole hole) :wildcards (ghost thief))
    (:difficulty 6 :colors 3 :hazards (base paddle wave) :wildcards nil)
    (:difficulty 6 :colors 4 :hazards (base paddle tracer) :wildcards nil)
    (:difficulty 7 :colors 3 :hazards (base hole paddle) :wildcards (thief))
    (:difficulty 7 :colors 4 :hazards (paddle tracer glitch) :wildcards (ghost))
    (:difficulty 8 :colors 3 :hazards (hole paddle base wave) :wildcards nil)
    (:difficulty 8 :colors 4 :hazards (base) :wildcards (boss))))

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

(defun level-wildcards (&optional (level *level*))
  (getf (nth level *levels*) :wildcards))

(defun bulkhead ()
  (new 'wall 200 20))

(defun make-hazard ()
  (let ((hazards (level-hazards)))
    (if hazards 
	(new (random-choose hazards))
	(bulkhead))))

(defun make-wildcard ()
  (let ((wildcards (level-wildcards)))
    (if wildcards (new (random-choose wildcards)) (bulkhead))))

(defun configure-level (level) 
  (assert (integerp level))
  (setf *level* level)
  (setf *difficulty* (level-difficulty level))
  (set-theme (level-theme *level*)))

