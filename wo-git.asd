;;;; wo-git.asd

(asdf:defsystem #:wo-git
  :serial t
  :depends-on (#:wo-graph
               #:wo-graph-functions
	       #:cl-ppcre
	       #:cl-git
	       #:wo-util
	       #:alexandria)
  :components ((:file "package")
               (:file "wo-git")))

