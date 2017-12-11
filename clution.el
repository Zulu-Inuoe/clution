(defvar *clution--current-clution* nil)
(defvar *clution--current-op* nil)
(defvar clution-build-complete-hook nil)

(defcustom clution-backend 'sbcl
  "The backend to use as default for clution."
  :type '(choice (const :tag "sbcl" sbcl)
                 (const :tag "roswell" roswell))
  :group 'clution)

(defvar clution-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-S-b") 'clution-build)
    (define-key map (kbd "<f5>") 'clution-run)
    (define-key map (kbd "C-<f5>") 'clution-run-no-debug)
    (define-key map (kbd "<f10>") 'clution-run-step)
    map))

(define-minor-mode clution-mode
  "minor mode for editing a clution project."
  t)

(define-derived-mode clution-output-mode fundamental-mode
  "clution-output"
  "Mode for the clution output buffer"
  (read-only-mode t))

(defun clution--output-buffer ()
  (let ((buffer (get-buffer-create "*clution-output*")))
    (with-current-buffer buffer
      (setf buffer-read-only t)
      (clution-output-mode))
    buffer))

(add-to-list 'purpose-user-mode-purposes '(clution-output-mode . clution-output))
(purpose-compile-user-configuration)

(defun clution--setup-idle ()
  (clution--output-buffer)
  (purpose-load-window-layout "clution-idle")
  (when-let ((out-window (get-buffer-window (clution--output-buffer))))
    (set-window-parameter out-window 'no-other-window t)))

(defun clution--setup-running ()
  (clution--output-buffer)
  (sly-start )
  (purpose-load-window-layout "clution-running")
  (when-let ((out-window (get-buffer-window (clution--output-buffer))))
    (set-window-parameter out-window 'no-other-window t)))

(defun clution--parse-string (str)
  (car (read-from-string str)))

(defun clution--parse-file (path)
  (clution--parse-string
   (with-temp-buffer
     (insert-file-contents path)
     (buffer-string))))

(defun clution--clution.name (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (getf (car clution) :name))

(defun clution--clution.path (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (cdr clution))

(defun clution--clution.systems (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (getf (car clution) :systems))

(defun clution--clution.selected-system (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (find
   (or (getf (car clution) :selected-system)
       (let ((sys (first (clution--clution.systems clution))))
         (if sys (clution--system.name sys) nil)))
   (clution--clution.systems clution)
   :key 'clution--system.name
   :test 'string-equal))

(defun clution--clution.output-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (or (getf (car clution) :output-dir)
       (concat
        (file-name-as-directory (file-name-directory (clution--clution.path)))
        "out"))))

(defun clution--clution.tmp-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (or (getf (car clution) :tmp-dir)
       (concat
        (file-name-as-directory (file-name-directory (clution--clution.path)))
        "tmp"))))

(defun clution--clution.asdf-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (concat
    (clution--clution.tmp-dir clution)
    "asdf")))

(defun clution--system.path (clution-system)
  (getf clution-system :path))

(defun clution--system.name (clution-system)
  (file-name-base (clution--system.path clution-system)))

(defun clution--system.toplevel (clution-system)
  (getf clution-system :toplevel))

(defun clution--spawn-lisp-command ()
  (ecase clution-backend
    (sbcl "sbcl")))

(defun clution--spawn-lisp-args ()
  (ecase clution-backend
    (sbcl
     '("--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
       "--noprint" "--disable-debugger" "--end-toplevel-options"))))

(defun clution--spawn-script-command ()
  (ecase clution-backend
    (sbcl "sbcl")))

(defun clution--spawn-script-args (script-path)
  (ecase clution-backend
    (sbcl
     `("--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
       "--noprint" "--disable-debugger" "--load" ,script-path "--eval" "(sb-ext:exit :code 0)"  "--end-toplevel-options"))))

(defun clution--clution.system-name (system)
  (or (downcase (file-name-base (clution--system.path system)))))

(defun clution--with-system-searcher (lispexpr)
  (let ((names-paths-alist
         (mapcar
          (lambda (system)
            (cons
             (clution--clution.system-name system)
             (expand-file-name
              (clution--system.path system)
              (file-name-directory (clution--clution.path)))))
          (clution--clution.systems)))
        (system-output-translations
         (mapcar
          (lambda (system)
            (list
             (concat
              (file-name-directory
               (expand-file-name
                (clution--system.path system)
                (file-name-directory (clution--clution.path))))
              "**/*.*")
             (concat
              (file-name-as-directory
               (concat
                (clution--clution.asdf-dir)
                (clution--system.name system)))
              "**/*.*")))
          (clution--clution.systems))))
    `(flet ((clution-system-searcher (system-name)
                                     (loop :for (name . path) :in ',names-paths-alist
                                           :if (string-equal system-name name)
                                           :return (parse-namestring path)))
            (clution-do ()
                        ,lispexpr))
       (let* ((asdf:*system-definition-search-functions*
               (list* (function clution-system-searcher)
                      asdf:*system-definition-search-functions*))
              (asdf:*output-translations-parameter* asdf:*output-translations-parameter*))
         (asdf:initialize-output-translations
          (append
           (list*
            :output-translations
            :inherit-configuration
            (mapcar
             (lambda (mapping)
               (mapcar #'parse-namestring mapping))
             ',system-output-translations))
           asdf:*output-translations-parameter*))
         (clution-do)))))

(defun clution--args-list-form ()
  (ecase clution-backend
    (sbcl
     'sb-ext:*posix-argv*)))

(defun clution--exit-form (exit-code-form)
  (ecase clution-backend
    (sbcl
     `(sb-ext:exit :code ,exit-code-form))))

(defun clution--build-form (system force)
  `(progn
     (handler-case
         ,(clution--with-system-searcher
           `(let* ((*standard-input* (make-string-input-stream "")))
              (asdf:compile-system ,(clution--system.name system) :force ,force :verbose nil)))
       (error ()
              ,(clution--exit-form 1)))
     ,(clution--exit-form 0)))

(defun clution--run-form (system)
  (let ((toplevel (clution--system.toplevel system))
        (system-name (clution--system.name system)))
    (clution--with-system-searcher
     `(progn
        (let ((*standard-output* (make-broadcast-stream))
              (*trace-output* (make-broadcast-stream)))
          (asdf:load-system ,system-name :verbose nil))

        (let ((ret-code (funcall (read-from-string ,toplevel) ,(clution--args-list-form))))
          (if (integerp ret-code)
              ,(clution--exit-form 'ret-code)
            ,(clution--exit-form 0)))))))

(defun clution--spawn-dir ()
  (file-name-directory (clution--clution.path)))

(defun clution--spawn-lisp (dir filter sentinel)
  (let* ((default-directory (file-name-directory dir))
         (proc
          (make-process
           :name "clution-spawn-lisp"
           :command (list* (clution--spawn-lisp-command) (clution--spawn-lisp-args))
           :connection-type nil
           :noquery nil
           :filter filter
           :sentinel sentinel)))
    (process-put proc 'clution--lisp-process t)
    proc))

(defun clution--arglist-to-string (arglist)
  (with-temp-buffer
    (dolist (s arglist)
      (insert " \"" (replace-regexp-in-string "\"" "\\\"" s) "\""))
    (buffer-string)))

(defun clution--spawn-script (dir script-path sentinel)
  (let* ((default-directory dir)
         (proc
          (start-process-shell-command
           "clution-spawn-script"
           nil
           (concat
            "start"
            " "
            (clution--spawn-script-command)
            " "
            (clution--arglist-to-string
             (clution--spawn-script-args script-path))))))
    (set-process-sentinel proc sentinel)))

(defun clution--eval-in-proc (proc sexpr)
  (process-send-string proc (format "%S\n" sexpr)))

(defun clution--clear-output ()
  (with-current-buffer (clution--output-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun clution--append-output (&rest args)
  (with-current-buffer (clution--output-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (apply 'insert args)))
  (when-let ((output-window (get-buffer-window (clution--output-buffer))))
    (with-selected-window output-window
      (set-window-point output-window (point-max)))))

(defvar *clution--build-remaining-systems* nil)

(defun clution--build-filter (proc string)
  (clution--append-output string))

(defun clution--build-sentinel (proc event)
  (case (process-status proc)
    (exit
     (let ((system (car *clution--build-remaining-systems*))
           (status (process-exit-status proc)))
       (clution--append-output "\n" (clution--system.name system) ": ")
       (if (zerop status)
           (clution--append-output "build completed successfully\n\n")
         (clution--append-output "error during build: process exited with code '" (number-to-string status) "'\n\n")))

     (pop *clution--build-remaining-systems*)

     (if (null *clution--build-remaining-systems*)
         (clution--build-complete)
       (clution--continue-build)))))

(defun clution--continue-build ()
  (let ((system (car *clution--build-remaining-systems*)))
    (clution--append-output
     (clution--system.name system) ": build starting"
     "'\n")

    (let ((build-proc (clution--spawn-lisp
                       (clution--spawn-dir)
                       'clution--build-filter
                       'clution--build-sentinel)))
      (clution--eval-in-proc build-proc (clution--build-form system nil)))))

(defun clution--kickoff-build (systems)
  (when *clution--current-op*
    (error "Already running clution operation '%S'" *clution--current-op*))

  (clution--append-output
   "Build starting: '" (clution--clution.name)
   "'\n\n")

  (setf *clution--current-op*
        (list
         :type 'clution-build
         :build-systems systems))
  (setf *clution--build-remaining-systems* systems)

  (clution--continue-build))

(defun clution--build-complete ()
  (clution--append-output
   "Build Complete: '" (clution--clution.name) "'\n")

  (setf *clution--current-op* nil)
  (run-hooks 'clution-build-complete-hook))

(defun clution--run-sentinel (proc event)
  (case (process-status proc)
    (exit
     (let ((status (process-exit-status proc)))
       (clution--append-output "Finished running. Exited with code " (number-to-string status) "(0x" (format "%x" status) ")\n\n")))))

(defun clution--run-on-build-complete ()
  (remove-hook 'clution-build-complete-hook 'clution--run-on-build-complete)

  (unless (clution--system.toplevel (clution--clution.selected-system))
    (error "Cannot run '%S', no toplevel defined" (clution--system.name (clution--clution.selected-system))))

  (clution--append-output
   "Running: '" (clution--clution.name)
   "' selected system: '" (clution--system.name (clution--clution.selected-system))
   "' toplevel: '" (clution--system.toplevel (clution--clution.selected-system))
   "'\n\n")

  (let* ((script-path
          (concat (clution--clution.output-dir)
                  (clution--system.name (clution--clution.selected-system))
                  "-script.lisp"))
         (script-dir (file-name-directory script-path)))
    (clution--append-output "Generating script " script-path "\n\n")

    (unless (file-exists-p script-dir)
      (make-directory script-dir))

    (write-region
     (pp-to-string (clution--run-form (clution--clution.selected-system)))
     nil
     script-path)

    (clution--append-output "Running script\n\n")
    (clution--spawn-script (clution--spawn-dir) script-path 'clution--run-sentinel)))

(defun clution-build ()
  (interactive)

  (cond
   (*clution--current-clution*
    (clution--clear-output)
    (clution--kickoff-build (clution--clution.systems)))
   (t
    (message "clution: no clution open"))))

(defun clution-run ()
  (interactive)

  (cond
   (*clution--current-clution*
    (clution--clear-output)
    (add-hook 'clution-build-complete-hook 'clution--run-on-build-complete)
    (clution--kickoff-build (list (clution--clution.selected-system))))
   (t
    (message "clution: no clution open")
    nil)))



(defun clution-open (path)
  (interactive
   (list (read-file-name "clution to open: " nil nil t)))

  (let ((path (expand-file-name path)))
    (setf *clution--current-clution*
          (cons (clution--parse-file path)
                path)))

  (clution--setup-idle))

(defun clution-close ()
  (interactive)
  (setf *clution--current-clution* nil))

(defun clution--find-file-hook ()
  (let ((path (buffer-file-name)))
    (when (and (null *clution--current-clution*)
               (string-match-p "^clu$" (file-name-extension path)))
      (clution-open path))))

(add-hook 'find-file-hook 'clution--find-file-hook)
