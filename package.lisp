(in-package #:cl-user)

(defpackage #:orgmark
  (:use #:cl #:asdf #:cl-org-mode)
  (:export :test
	   :org-parse))
