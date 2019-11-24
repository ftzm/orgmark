(asdf:defsystem #:orgmark
  :description "manage org bookmarks"
           :version "0.0.1"
  :depends-on (#:cl-org-mode)
  :serial t
  :components ((:file "package")
	       (:file "orgmark")))
