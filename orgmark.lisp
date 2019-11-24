(in-package #:orgmark)

(defun test ()
  (print "Orgmark lives!"))

(defun file-get-contents (filename)
  "Provide file contents as string."
  (declare (string filename))
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (declare (string contents))
      (read-sequence contents stream)
      contents)))

(defvar marks (file-get-contents "/home/matt/org/bookmarks.org"))

(defvar orgdoc (org-parse marks))

(org-present :normal orgdoc *standard-output*)
