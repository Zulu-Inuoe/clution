(defvar *clution-current-clution* nil)
(defvar *clution-current-clution-path* nil)

(define-minor-mode clution-mode
  "minor mode for editing a clution project."
  :global t)

(defvar clution-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-S-b") 'clution-build)
    (define-key map (kbd "<f5>") 'clution-run-debug)
    (define-key map (kbd "C-<f5>") 'clution-run-no-debug)
    (define-key map (kbd "<f10>") 'clution-run-step)
    map))

(define-derived-mode clution-output-mode fundamental-mode
  "clution-output"
  "Mode for the clution output buffer"
  (read-only-mode t))

(defun clution-browser-buffer ()
  (let ((buffer (neotree-toggle)))))

(defun clution-output-buffer ()
  (let ((buffer (get-buffer-create "*clution-output*")))
    (with-current-buffer buffer
      (clution-output-mode))
    buffer))

(add-to-list 'purpose-user-mode-purposes '(clution-output-mode . clution-output))
(purpose-compile-user-configuration)

(defun clution-setup-idle ()
  (clution-output-buffer)
  (purpose-load-window-layout "clution-idle"))

(defun clution-setup-running ()
  (clution-output-buffer)
  (sly)
  (purpose-load-window-layout "clution-running"))

(defun clution-parse-string (str)
  (car (read-from-string str)))

(defun clution-parse-file (path)
  (clution-parse-string
   (with-temp-buffer
     (insert-file-contents path)
     (buffer-string))))

(defun clution-system.path (clution-system)
  (getf clution-system :path))

(defun clution-system.toplevel (clution-system)
  (getf clution-system :toplevel))

(defun clution-system.name (clution-system)
  (file-name-base (clution-system.path clution-system)))

(defun clution.selected-system (&optional clution)
  (unless clution
    (setf clution *clution-current-clution*))
  (or (getf clution :selected-system)
      (let ((sys (first (clution.systems clution))))
        (if sys (clution-system.name sys) nil))))

(defun clution.name (&optional clution)
  (unless clution
    (setf clution *clution-current-clution*))

  (getf clution :name))

(defun clution.systems (&optional clution)
  (unless clution
    (setf clution *clution-current-clution*))

  (getf clution :systems))

(defun clution-spawn-lisp-command ()
  "sbcl")

(defun clution-spawn-lisp-args ()
  '("--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options" "--noprint" "--disable-debugger"  "--end-toplevel-options"))

(defun clution-escape-string (str)
  (concat "\"" str "\""))

(defun clution.system-name (system)
  (or (downcase (file-name-base (clution-system.path system)))))

(defun clution-asd-path-to-system-name (system)
  (cons
   (clution-escape-string (clution.system-name system))
   (clution-escape-string (expand-file-name
                           (clution-system.path system)
                           (file-name-directory *clution-current-clution-path*)))))

(defun clution-build-form-str (systems-to-build force)
  "String to execute on lisp to perform the build operation"
  (let ((clution-systems-and-dirs
         (mapcar 'clution-asd-path-to-system-name
                 (clution.systems *clution-current-clution*)))
        (build-systems
         (mapcar 'clution-escape-string
                 (if (listp systems-to-build)
                     systems-to-build
                   (list systems-to-build)))))
    (format
     "
(progn
  (format t \"Build started~%%~%%\")
  (ignore-errors
    (flet ((clution-project-searcher (system-name)
                                     \"This function is added to ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*
                                     to use the clution systems to find systems.\"
                                     (loop :for (name . path) :in '%s
                                           :if (string-equal system-name name)
                                           :return (parse-namestring path))))
      (let* ((*standard-input* (make-string-input-stream \"\"))
             (asdf:*system-definition-search-functions*
              (list* #'clution-project-searcher
                     asdf:*system-definition-search-functions*)))
        (dolist (sys '%s)
          (format t \"Compiling system: ~A~%%~%%\" sys)
          (asdf:compile-system sys :force %s :verbose nil)
          (format t \"~%%Finished compiling system: ~A~%%\" sys)))))
  (format t \"~%%Build completed.~%%\")
  (finish-output)
  (exit :code 0))"
clution-systems-and-dirs
build-systems
force)))

(defun clution-run-form-str (system-name)
  (let* ((clution-systems-and-dirs
          (mapcar 'clution-asd-path-to-system-name
                  (clution.systems)))
         (run-system (find system-name (clution.systems) :key 'clution.system-name :test 'string-equal))
         (toplevel (clution-system.toplevel run-system))
         toplevel-package
         toplevel-symbol)

    (when toplevel
      (string-match "^\\([^:]+\\)::?\\([^:]+\\)$" toplevel)
      (let ((package (match-string 1 toplevel))
            (symbol  (match-string 2 toplevel)))
        (if (and package symbol)
            (setf toplevel-package package
                  toplevel-symbol symbol)
          (error "toplevel malformed: %s, expected package:symbol or package::symbol" toplevel))))
    (format
     "
(progn
  (flet ((clution-project-searcher (system-name)
                                   \"This function is added to ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*
                                   to use the clution systems to find systems.\"
                                   (loop :for (name . path) :in '%s
                                         :if (string-equal system-name name)
                                         :return (parse-namestring path))))
    (let* ((asdf:*system-definition-search-functions*
            (list* #'clution-project-searcher
                   asdf:*system-definition-search-functions*)))
      (asdf:load-system %s)))
  (when %s
    (let ((ret-code (funcall (find-symbol %s %s))))
      (if (integerp ret-code)
        (exit :code ret-code)
        (exit :code 0)))))
"
clution-systems-and-dirs
(clution-escape-string system-name)
(and toplevel t)
(clution-escape-string (upcase toplevel-symbol))
(clution-escape-string (upcase toplevel-package)))))

(defun clution-spawn-dir ()
  (file-name-directory *clution-current-clution-path*))

(defun clution-spawn-lisp (proc-name program args dir)
  (let* ((default-directory (file-name-directory dir))
         (proc
          (make-process
           :name proc-name
           :command (list* program args)
           :connection-type nil
           :noquery nil)))
    (process-put proc 'clution-lisp-process t)
    proc))

(defun clution-eval-in-proc (proc lisp-str)
  (process-send-string proc lisp-str)
  (process-send-string proc "
"))

(defun clution-build-process-filter (proc string)
  (with-current-buffer (clution-output-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert string))))

(defun clution-build ()
  (interactive)
  (with-current-buffer (clution-output-buffer)
    (setf buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Build starting: '" (clution.name) "' selected system: '" (clution.selected-system) "'\n\n"))
    (let ((build-proc (clution-spawn-lisp
                       "clution-build"
                       (clution-spawn-lisp-command)
                       (clution-spawn-lisp-args)
                       (clution-spawn-dir))))
      ;;Give a little bit for the process to start
      (set-process-filter build-proc 'clution-build-process-filter)
      (clution-eval-in-proc build-proc (clution-build-form-str (clution.selected-system) nil))
      (accept-process-output build-proc 5))))

(defun clution-run ()
  (interactive)
  (clution-setup-running)

  (with-current-buffer (clution-output-buffer)
    (erase-buffer)
    (sly-eval-print
     (clution-run-form-str (clution.selected-system)))))

(defun clution-open (path)
  (interactive
   (list (read-file-name "clution to open: " nil nil t)))

  (setf *clution-current-clution-path* (expand-file-name path))
  (setf *clution-current-clution* (clution-parse-file *clution-current-clution-path*))

  (clution-setup-idle))

(defun clution-close ()
  (interactive)
  (setf *clution-current-clution* nil))
