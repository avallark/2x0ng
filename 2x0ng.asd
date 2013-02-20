(asdf:defsystem #:2x0ng
  :depends-on (:blocky)
  :components ((:file "package")
               (:file "things" :depends-on ("package"))
	       (:file "robot" :depends-on ("things"))
               (:file "level" :depends-on ("robot"))
	       (:file "2x0ng" :depends-on ("level"))))
