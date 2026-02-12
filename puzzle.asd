;;;; puzzle.asd

(asdf:defsystem #:puzzle
  :description "Describe puzzle here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-liballegro #:alexandria)
  :components ((:file "package")
	       (:file "algorithms")
	       (:file "piece")
	       (:file "board")
               (:file "puzzle")))
