(in-package #:cl-user)

(defpackage #:orgmark
  (:use #:cl #:cl-org-mode #:cl-cli)
  (:export :main
	   :org-parse))
