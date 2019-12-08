(in-package #:orgmark)

;; Util

(defun file-get-contents (filename)
  "Provide file contents as string."
  (declare (string filename))
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (declare (string contents))
      (read-sequence contents stream)
      contents)))

(defmethod writeout ((doc org-document) dir)
  (with-open-file
      (stream dir :direction :output :if-exists :overwrite)
    (org-present :normal doc stream)))

;; Create mark

(defun make-mark (name url)
  (make-node name nil nil :static-properties (list (cons "URL" url)))
  )

(defmethod add-mark ((doc org-document) name url)
  (with-accessors ((children node.out)) doc
    (setf children (append children (list (make-mark name url)))))
  doc)

(defun create-mark (dir name url)
  (let* ((doc (org-parse (file-get-contents dir)))
	 (new-doc (add-mark doc name url)))
    (writeout new-doc dir)))


;; Print marks

(defun print-marks (dir)
  (let ((marks (node.out (org-parse (file-get-contents dir)))))
    (mapc
     #'(lambda (mark)
	 (let ((title (title-of mark))
	       (url (cdr (assoc
			  "URL"
			  (static-properties-of mark)
			  :test #'string=))))
	   (format t "~a~a~a~%" title #\tab url)))
     marks)))

;; CLI

(defparameter top-options
  (list '(*dir* nil "The directory of the bookmarks file. Mandatory." :params ("DIR"))))

(defparameter sub-commands
  (list (defcommand
	    ("add")
	    ((name nil "bookmark name")
	     (url nil "bookmark url"))
	    "add a bookmark"
	  (create-mark *dir* name url))))

(defun run-command-strict (argv options commands)
  (multiple-value-bind (opts-vars opts-values sub-func sub-opts argv)
      (parse-cli argv options commands)
    (progv opts-vars opts-values
      (when (not *dir*)
	(format t "Must specify: --dir <bookmark dir>~%")
	(return-from run-command-strict))
      (when (not sub-func)
	(format t "Must specify a command.~%")
	(return-from run-command-strict))
      (progv '(doc) (list (file-get-contents *dir*))
	(apply sub-func sub-opts)))))

(defun main ()
  (handler-case (run-command-strict sb-ext:*posix-argv* top-options sub-commands)
    (cl-cli::option-requires-argument (c)
      (format t "~a~%" c)
      (sb-ext:exit :code 1))
    (cl-cli::bad-argument-type (c)
      (format t "~a~%" c)
      (sb-ext:exit :code 1))
    (cl-cli::not-enougth-pos-args (c)
      (format t "~a~%" c)
      (sb-ext:exit :code 1))
    (common-lisp:file-error (c)
      (format t "~a~%" c)
      (sb-ext:exit :code 1))))

(defun test () (run-command '("add") top-options sub-commands))
