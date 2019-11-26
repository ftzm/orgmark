(asdf:defsystem #:orgmark
  :description "manage org bookmarks"
           :version "0.0.1"
  :depends-on (#:cl-org-mode #:cl-cli)
  :serial t
  :components ((:file "package")
	       (:file "orgmark"))
  ;; build stuff
  :build-operation "program-op" ;; leave as is
  :build-pathname "orgmark"
  :entry-point "orgmark:main"
  )
