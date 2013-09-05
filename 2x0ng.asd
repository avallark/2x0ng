(asdf:defsystem #:2x0ng
  :depends-on (:xelf)
  :components ((:file "package")
	       (:file "basic" :depends-on ("package"))
               (:file "things" :depends-on ("basic"))
	       (:file "robot" :depends-on ("things"))
	       (:file "enemy" :depends-on ("robot"))
               (:file "level" :depends-on ("robot"))
               (:file "story" :depends-on ("level"))
	       (:file "2x0ng" :depends-on ("story"))))
