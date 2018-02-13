(in-package #:cl-clution)

(defun clu-plist (clu-path)
  (when-let* ((clu (read-clu-file clu-path)))
    (clu-file-plist clu)))

(defun create-clu (clu-path)
  (let ((clu (make-instance 'clu-file :path clu-path)))
    (ensure-directories-exist clu-path)
    (with-output-to-file (stream clu-path :if-exists :supersede :external-format :utf-8)
      (write-clu-file clu stream)))
  t)

(defun add-clu-dir (clu-path item-path dir-name)
  (when-let* ((clu (read-clu-file clu-path)))
    (clu-file-add-dir clu item-path dir-name)
    (with-output-to-file (stream clu-path :if-exists :supersede :external-format :utf-8)
      (write-clu-file clu stream))
    t))

(defun remove-clu-dir (clu-path item-path)
  (when-let* ((clu (read-clu-file clu-path)))
    (clu-file-remove-dir clu item-path)
    (with-output-to-file (stream clu-path :if-exists :supersede :external-format :utf-8)
      (write-clu-file clu stream))
    t))

(defun add-clu-system (clu-path item-path system-path system-type)
  (when-let* ((clu (read-clu-file clu-path)))
    (clu-file-add-system clu item-path system-path system-type)
    (with-output-to-file (stream clu-path :if-exists :supersede :external-format :utf-8)
      (write-clu-file clu stream))
    t))

(defun remove-clu-system (clu-path item-path system-path)
  (when-let* ((clu (read-clu-file clu-path)))
    (clu-file-remove-system clu item-path system-path)
    (with-output-to-file (stream clu-path :if-exists :supersede :external-format :utf-8)
      (write-clu-file clu stream))
    t))

(defun has-clu-qlfiles (clu-path)
  (when-let* ((clu (read-clu-file clu-path)))
    (and (clu-has-qlfiles clu)
         t)))

(defun are-clu-qlfile-libs-up-to-date (clu-path)
  (when-let* ((clu (read-clu-file clu-path)))
    (and (clu-qlfile-libs-up-to-date clu)
         t)))

(defun sync-clu-qlfiles (clu-path)
  (when-let* ((clu (read-clu-file clu-path)))
    (clu-sync-qlfiles clu)
    t))

(defun asd-plists (asd-path)
  "Gets the plist of the first system found in `asd-path'"
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-system-plists asd)))

(defun add-file-component (asd-path component-path component-name)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-add-file-component asd component-path component-name)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun add-module-component (asd-path component-path component-name)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-add-module-component asd component-path component-name)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun add-static-file-component (asd-path component-path component-name)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-add-static-file-component asd component-path component-name)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun rename-component (asd-path component-path new-name)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-rename-component asd component-path new-name)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun move-component-up (asd-path component-path)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-move-component-up asd component-path)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun move-component-down (asd-path component-path)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-move-component-down asd component-path)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun remove-component (asd-path component-path)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-remove-component asd component-path)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun add-depends-on (asd-path component-path dependency-name)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-add-depends-on asd component-path (make-symbol (string-upcase dependency-name)))
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun remove-depends-on (asd-path component-path dependency-name)
  (when-let* ((asd (read-asd-file asd-path)))
    (asd-file-remove-depends-on asd component-path dependency-name)
    ;;Spit out the new system file
    (with-output-to-file (stream asd-path :if-exists :supersede :external-format :utf-8)
      (write-asd-file asd stream))
    t))

(defun main (&rest args)
  (unless (= (length args) 1)
    (format t "Invalid arguments~%")
    (return-from main -1))

  (let ((terminator (first args))
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
            (let ((*print-case* :downcase))
              (print result clution-output))

            ;;Write the terminator
            (write-string terminator clution-output)
            (finish-output clution-output))))))
  0)
