(in-package #:cl-clution)

(defvar *active-asd-files* ())

(defun get-asd-from-path (path)
  (cdr (assoc (truename path) *active-asd-files* :test #'uiop:pathname-equal)))

(defun set-active-asd-files (&rest paths)
  (setf *active-asd-files*
        (mapcar (lambda (p)
                  (let ((truename (truename p)))
                    (cons truename (read-asd-file truename))))
                paths))
  t)

(defun asd-plists (asd-path)
  "Gets the plist of the first system found in `asd-path'"
  (when-let* ((asd (read-asd-file asd-path)))
    (-> (asd-file-systems asd)
        (select (lambda (s) (system-plist s asd-path)))
        (to-list))))

(defun rename-component (asd-path component-path new-name
                         &aux
                           (system-name (first component-path)))
  (declare (ignore system-name))
  (when-let* ((asd (read-asd-file asd-path))
              ;;TODO need to find system by name
              (system (efirst (asd-file-systems asd))))
    (system-rename-component system (rest component-path) new-name)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun remove-component (asd-path component-path
                         &aux
                           (system-name (first component-path)))
  (declare (ignore system-name))
  (when-let* ((asd (read-asd-file asd-path))
              ;;TODO need to find system by name
              (system (efirst (asd-file-systems asd))))
    (system-remove-component system (rest component-path))
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun main (&rest args)
  (unless (= (length args) 2)
    (format t "Invalid arguments~%")
    (return-from main -1))

  (let ((terminator (second args))
        (eof-value (gensym)))
    ;;rebind all the IO
    (let* ((clution-input *standard-input*)
           (clution-output *standard-output*)
           (*standard-input* (make-string-input-stream ""))
           (*standard-output* (make-broadcast-stream))
           (*error-output* *standard-output*)
           (*trace-output* *standard-output*)
           (*debug-io* (make-two-way-stream *standard-input*  *standard-output*))
           (*query-io* *debug-io*)
           (*package* (find-package "CL-CLUTION")))
      ;;Start the repl
      (let (form result)
        (loop
          (when (handler-case
                    (progn
                      (setf form (read clution-input nil eof-value))
                      t)
                  (error ()
                    nil))
            (when (eq form eof-value)
              (return))

            (handler-case
                (setf result (cons :success (eval form)))
              (error (err)
                (setf result (cons :fail (princ-to-string err)))))

            ;;Print the result
            (print result clution-output)

            ;;Write the terminator
            (write-string terminator clution-output)
            (finish-output clution-output))))))
  0)
