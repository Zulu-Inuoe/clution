(defvar *clution--current-clution* nil)
(defvar *clution--current-watch* nil)
(defvar *clution--repl-active* nil)
(defvar *clution--current-op* nil)

(defun clution--output-buffer ()
  (let ((buffer (get-buffer-create "*clution-output*")))
    (with-current-buffer buffer
      (setf buffer-read-only t)
      (clution-output-mode))
    buffer))

(defun clution--parse-string (str)
  (let ((clution (cons (car (read-from-string str)) nil)))

    (setf (getf (car clution) :systems)
          (mapcar
           (lambda (sys)
             (cons sys clution))
           (getf (car clution) :systems)))

    clution))

(defun clution--parse-file (path)
  (let ((ret
         (clution--parse-string
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string)))))
    (setf (cdr ret) path)
    ret))

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
        (file-name-as-directory (file-name-directory (clution--clution.path clution)))
        "out"))))

(defun clution--clution.tmp-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (or (getf (car clution) :tmp-dir)
       (concat
        (file-name-as-directory (file-name-directory (clution--clution.path clution)))
        "tmp"))))

(defun clution--clution.script-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (concat
    (clution--clution.tmp-dir clution)
    "script")))

(defun clution--clution.asdf-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (concat
    (clution--clution.tmp-dir clution)
    "asdf")))

(defun clution--clution.dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))
  (file-name-directory (clution--clution.path clution)))

(defun clution--system.path (clution-system)
  (getf (car clution-system) :path))

(defun clution--system.name (clution-system)
  (downcase (file-name-base (clution--system.path clution-system))))

(defun clution--system.clution (clution-system)
  (cdr clution-system))

(defun clution--system.toplevel (clution-system)
  (getf (car clution-system) :toplevel))

(defun clution--system.startup-dir (clution-system)
  (if-let ((dir (getf (car clution-system) :startup-dir)))
      (file-name-as-directory
       (expand-file-name dir (clution--clution.path (clution--system.clution clution-system))))
    (clution--clution.dir (clution--system.clution clution-system))))

(defun clution--system.args (clution-system)
  (getf (car clution-system) :args))

(defun clution--system.script-path (clution-system)
  (concat
   (file-name-as-directory
    (concat
     (clution--clution.script-dir (clution--system.clution clution-system))
     (clution--system.name clution-system)))
   (clution--system.name clution-system)
   "-script.lisp"))

(defun clution--spawn-lisp-command ()
  (ecase clution-frontend
    (raw
     (ecase clution-backend
       (sbcl "sbcl")))
    (roswell "ros")))

(defun clution--spawn-lisp-args ()
  (ecase clution-frontend
    (raw
     (ecase clution-backend
       (sbcl
        '("--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
          "--noprint" "--disable-debugger"))))
    (roswell
     (ecase clution-backend
       (sbcl
        '("run" "--lisp" "sbcl-bin" "--"
          "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
          "--noprint" "--disable-debugger"))))))

(defun clution--spawn-script-command ()
  (ecase clution-frontend
    (raw
     (ecase clution-backend
       (sbcl "sbcl")))
    (roswell "ros")))

(defun clution--spawn-script-args (system)
  (let ((script-path (clution--system.script-path system))
        (script-args (clution--system.args system)))
    (ecase clution-frontend
      (raw
       (ecase clution-backend
         (sbcl
          `("--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
            "--noprint" "--disable-debugger" "--load" ,script-path "--eval" "(sb-ext:exit :code 0)"  "--end-toplevel-options"
            ,@script-args))))
      (roswell
       (ecase clution-backend
         (sbcl
          `("run" "--lisp" "sbcl-bin" "--"
            "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
            "--noprint" "--disable-debugger" "--load" ,script-path "--eval" "(sb-ext:exit :code 0)"  "--end-toplevel-options"
            ,@script-args)))))))

(defun clution--spawn-repl-command ()
  (ecase clution-frontend
    (raw
     (ecase clution-backend
       (sbcl "sbcl")))
    (roswell "ros")))

(defun clution--spawn-repl-args ()
  (ecase clution-frontend
    (raw
     (ecase clution-backend
       (sbcl `())))
    (roswell
     (ecase clution-backend
       (sbcl '("run" "--lisp" "sbcl-bin"))))))

(defun clution--with-system-searcher (clution lispexpr)
  (let ((names-paths-alist
         (mapcar
          (lambda (system)
            (cons
             (clution--system.name system)
             (expand-file-name
              (clution--system.path system)
              (file-name-directory (clution--clution.path clution)))))
          (clution--clution.systems clution)))
        (system-output-translations
         (mapcar
          (lambda (system)
            (list
             (concat
              (file-name-directory
               (expand-file-name
                (clution--system.path system)
                (file-name-directory (clution--clution.path clution))))
              "**/*.*")
             (concat
              (file-name-as-directory
               (concat
                (clution--clution.asdf-dir clution)
                (clution--system.name system)))
              "**/*.*")))
          (clution--clution.systems clution))))
    `(cl:flet ((clution-system-searcher (system-name)
                                     (cl:loop :for (name . path) :in ',names-paths-alist
                                           :if (cl:string-equal system-name name)
                                           :return (cl:parse-namestring path)))
            (clution-do ()
                        ,lispexpr))
       (cl:let* ((asdf:*system-definition-search-functions*
               (cl:list* (cl:function clution-system-searcher)
                      asdf:*system-definition-search-functions*))
              (asdf:*output-translations-parameter* asdf:*output-translations-parameter*))
         (asdf:initialize-output-translations
          (cl:append
           (cl:list*
            :output-translations
            :inherit-configuration
            (cl:mapcar
             (cl:lambda (mapping)
               (cl:mapcar (cl:function cl:parse-namestring) mapping))
             ',system-output-translations))
           asdf:*output-translations-parameter*))
         (clution-do)))))

(defun clution--args-list-form ()
  (ecase clution-backend
    (sbcl
     '(cdr sb-ext:*posix-argv*))))

(defun clution--exit-form (exit-code-form)
  (ecase clution-backend
    (sbcl
     `(sb-ext:exit :code ,exit-code-form))))

(defun clution--build-form (system force)
  `(progn
     (handler-case
         ,(clution--with-system-searcher (clution--system.clution system)
           `(let* ((*standard-input* (make-string-input-stream "")))
              (asdf:compile-system ,(clution--system.name system) :force ,force :verbose nil)))
       (error (e)
              (format *error-output* "error during build:~%~T~A" e)
              ,(clution--exit-form 1)))
     ,(clution--exit-form 0)))

(defun clution--run-form (system)
  (let ((system-name (clution--system.name system))
        (toplevel (clution--system.toplevel system)))
    (clution--with-system-searcher (clution--system.clution system)
     `(progn
        (let ((*standard-output* (make-broadcast-stream))
              (*trace-output* (make-broadcast-stream)))
          (asdf:load-system ,system-name :verbose nil))

        (handler-case
            (let ((ret-code (apply (read-from-string ,toplevel) ,(clution--args-list-form))))
              (if (integerp ret-code)
                  ,(clution--exit-form 'ret-code)
                ,(clution--exit-form 0)))
          (error (e)
                 (format *error-output* "Uncaught error while running:~%~T~A" e)
                 ,(clution--exit-form 1)))))))

(defun clution--repl-form (clution)
  (let ((system-names (mapcar 'clution--system.name (clution--clution.systems clution))))
    (clution--with-system-searcher clution
     `(progn
        (let ((*standard-output* (make-broadcast-stream))
              (*trace-output* (make-broadcast-stream)))
          (dolist (system-name ',system-names)
            (asdf:load-system system-name :verbose nil)))))))

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
      (insert " ^\""
              (replace-regexp-in-string
               "\n" "^\n\n"
               (replace-regexp-in-string
                "\\((\\|)\\|%\\|!\\|\\^\\|\"\\|<\\|>\\|&\\||\\)"
                "^\\1"
                (replace-regexp-in-string "\"" "\\\\\"" s)))
              "^\""))
    (buffer-string)))

(defun clution--spawn-script (system sentinel)
  (let* ((default-directory (clution--system.startup-dir system))
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
             (clution--spawn-script-args system))))))
    (set-process-sentinel proc sentinel)))

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
         (clution--append-output "error during build: process exited with code '"
                                 (number-to-string status) "'\n\n")))

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
                       (clution--system.startup-dir system)
                       'clution--build-filter
                       'clution--build-sentinel)))
      (process-send-string build-proc
                           (format "%S\n" (clution--build-form system nil))))))

(defun clution--kickoff-build (systems)
  (clution--append-output "Building systems:\n")
  (dolist (system systems)
    (clution--append-output "  " (clution--system.name system) "\n"))
  (clution--append-output "\n")

  (setf *clution--current-op*
        (list
         :type 'clution-build
         :build-systems systems))
  (setf *clution--build-remaining-systems* systems)

  (clution--continue-build))

(defun clution--repl-sly-compile (systems)
  (let* ((clution (clution--system.clution (car systems)))
         (system-names (mapcar 'clution--system.name systems)))
    (sly-eval-async
        `(cl:progn
          ,(clution--with-system-searcher clution
                                          `(slynk::collect-notes
                                            (cl:lambda ()
                                                       (cl:dolist (system-name ',system-names)
                                                                  (cl:handler-case
                                                                   (slynk::with-compilation-hooks ()
                                                                                                  (asdf:compile-system system-name :force t))
                                                                   (asdf:compile-error ()
                                                                                       nil)
                                                                   (asdf/lisp-build:compile-file-error ()
                                                                                                       nil)))))))

      (lexical-let ((clution clution))
        (lambda (result)
          (let ((default-directory (clution--clution.dir clution)))
            (sly-compilation-finished result nil)
            (clution--append-output (with-current-buffer (sly-buffer-name :compilation) (buffer-string)))
            (clution--build-complete))))
      "COMMON-LISP-USER")))

(defun clution--kickoff-build-in-repl (systems)
  (clution--append-output "Building systems:\n")
  (dolist (system systems)
    (clution--append-output "  " (clution--system.name system) "\n"))
  (clution--append-output "\n")

  (setf *clution--current-op*
        (list
         :type 'clution-build-in-repl
         :build-systems systems))

  (let ((system-names (mapcar 'clution--system.name systems)))
    (ecase clution-repl-style
      (sly
       (clution--repl-sly-compile systems)))))

(defun clution--build-complete ()
  (clution--append-output
   "Build Complete: '" (clution--clution.name) "'\n")

  (setf *clution--current-op* nil)
  (run-hooks 'clution-build-complete-hook))

(defun clution--run-sentinel (proc event)
  (case (process-status proc)
    (exit
     (let ((status (process-exit-status proc)))
       (clution--append-output "Finished running. Exited with code " (number-to-string status) "(0x" (format "%x" status) ")\n\n"))
     (clution--run-complete))))

(defun clution--run-complete ()
  (setf *clution--current-op* nil)
  (run-hooks 'clution-run-complete-hook))

(defun clution--run-on-build-complete ()
  (remove-hook 'clution-build-complete-hook 'clution--run-on-build-complete)

  (unless (clution--system.toplevel (clution--clution.selected-system))
    (error "Cannot run '%S', no toplevel defined" (clution--system.name (clution--clution.selected-system))))

  (setf *clution--current-op*
        (list
         :type 'clution-run
         :toplevel (clution--system.toplevel (clution--clution.selected-system))))

  (clution--append-output
   "Running: '" (clution--clution.name)
   "' selected system: '" (clution--system.name (clution--clution.selected-system))
   "' toplevel: '" (clution--system.toplevel (clution--clution.selected-system))
   "'\n\n")

  (let* ((system (clution--clution.selected-system))
         (script-path (clution--system.script-path system))
         (script-dir (file-name-directory script-path)))
    (clution--append-output "Generating script " script-path "\n\n")

    (unless (file-exists-p script-dir)
      (make-directory script-dir t))

    (write-region
     (pp-to-string (clution--run-form system))
     nil
     script-path)

    (clution--append-output "Running script\n\n")
    (clution--spawn-script system 'clution--run-sentinel)))

(defun clution--do-clean (systems)
  (dolist (system systems)
    (let ((system-cache-dir (file-name-as-directory
                             (concat
                              (clution--clution.asdf-dir)
                              (clution--system.name system)))))
      (when (file-exists-p system-cache-dir)
        (clution--append-output
         "Removing '" (clution--system.name system)
         "' asdf cache: '" system-cache-dir
         "'\n\n")

        (delete-directory system-cache-dir t)))))

(defun make-setup-script (clution setup-form))

(defun make-init-script (clution init-form))

(defun clution--start-repl ()
  (clution--append-output
   "\nClution REPL starting with style '" (format "%s" clution-repl-style) "'\n")
  (ecase clution-repl-style
    (sly
     (lexical-let (net-close-hook)
       (setf net-close-hook
             (lambda (proc)
               (remove-hook 'sly-net-process-close-hooks net-close-hook)
               (clution--repl-exited)))
       (add-hook 'sly-net-process-close-hooks net-close-hook))

     (sly-start
      :program (clution--spawn-repl-command)
      :program-args (clution--spawn-repl-args)
      :directory (clution--clution.dir)
      :init (lambda (port-filename coding-system)
              (format "(progn %s %S)\n\n"
                      (funcall sly-init-function port-filename coding-system)
                      (clution--repl-form *clution--current-clution*))))

     (setf *clution--repl-active* t))))

(defun clution--repl-sentinel (proc event)
  (case (process-status proc)
    (exit
     (clution--repl-exited))))

(defun clution--repl-exited ()
  (clution--append-output "\nclution-repl: repl exited\n")
  (setf *clution--repl-active* nil))

(defun clution--find-file-hook ()
  (let ((path (buffer-file-name)))
    (when (and (null *clution--current-clution*)
               (string-match-p "^clu$" (file-name-extension path))
               (file-exists-p path))
      (clution-open path))))

(defun clution--file-watch-callback (event)
  (destructuring-bind (descriptor action file &optional file1)
      event
    (case action
      (created)
      (deleted
       (clution-close))
      (changed
       (message "Reloading clution (changed %S)" file)
       (setf *clution--current-clution* (clution--parse-file file)))
      (renamed
       (message "Reloading clution (rename to %S)" file1)
       (setf *clution--current-clution* (clution--parse-file file1)))
      (attribute-changed)
      (stopped))))

(defvar clution-open-hook nil
  "Hook executed whenever a clution is opened.")

(defvar clution-close-hook nil
  "Hook executed whenever a clution is closed.")

(defvar clution-build-started-hook nil
  "Hook executed whenever a 'build' operation begins.")

(defvar clution-build-complete-hook nil
  "Hook executed whenever a 'build' operation completes.")

(defvar clution-run-started-hook nil
  "Hook executed whenever a 'run' operation begins.")

(defvar clution-run-complete-hook nil
  "Hook executed whenever a 'run' operation completes.")

(defcustom clution-frontend 'roswell
  "The frontend to use as default for clution."
  :type '(choice (const :tag "Use clution-backend directly" raw)
                 (const :tag "Roswell" roswell))
  :group 'clution)

(defcustom clution-backend 'sbcl
  "The backend to use as default for clution."
  :type '(choice (const :tag "sbcl" sbcl))
  :group 'clution)

(defcustom clution-repl-style 'sly
  "The type of repl to use for clution"
  :type '(choice (const :tag "Use Sly" sly))
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

(define-derived-mode clution-file-mode lisp-mode
  "clution-file"
  "Major mode for editing a clution project file.")

(define-derived-mode clution-output-mode compilation-mode
  "clution-output"
  "Mode for the clution output buffer"
  (read-only-mode t))

(defun clution-repl ()
  (interactive)

  (cond
   (*clution--current-op*
    (message "clution: busy doing op: '%s'" (getf *clution--current-op* :type)))
   (*clution--repl-active*
    (message "clution: repl already active"))
   (*clution--current-clution*
    (clution--clear-output)
    (clution--append-output
     "Starting repl for: '" (clution--clution.name *clution--current-clution*)
     "'\n\n")
    (clution--start-repl))
   (t
    (message "clution: no clution open"))))

(defun clution-build ()
  (interactive)

  (cond
   (*clution--current-op*
    (message "clution: busy doing op: '%s'" (getf *clution--current-op* :type)))
   (*clution--current-clution*
    (clution--clear-output)
    (clution--append-output
     "Build starting: '" (clution--clution.name *clution--current-clution*)
     "'\n\n")
    (cond
     (*clution--repl-active*
      (clution--kickoff-build-in-repl (clution--clution.systems)))
     (t
      (clution--kickoff-build (clution--clution.systems)))))
   (t
    (message "clution: no clution open"))))

(defun clution-run ()
  (interactive)

  (cond
   (*clution--current-op*
    (message "clution: busy doing op: '%s'" (getf *clution--current-op* :type)))
   (*clution--current-clution*
    (clution--clear-output)
    (add-hook 'clution-build-complete-hook 'clution--run-on-build-complete)
    (clution--append-output
     "Build starting: '" (clution--clution.name *clution--current-clution*)
     "'\n\n")
    (clution--kickoff-build (list (clution--clution.selected-system))))
   (t
    (message "clution: no clution open"))))

(defun clution-clean ()
  (interactive)

  (cond
   (*clution--current-op*
    (message "clution: busy doing op: '%s'" (getf *clution--current-op* :type)))
   (*clution--current-clution*
    (clution--clear-output)
    (clution--append-output
     "Clean starting: '" (clution--clution.name)
     "'\n\n")

    (clution--do-clean (clution--clution.systems))

    (clution--append-output
     "Clean complete: '" (clution--clution.name)
     "'\n"))
   (t
    (message "clution: no clution open")
    nil)))

(defun clution-open (path)
  (interactive
   (list (read-file-name "clution to open: " nil nil t)))

  (when *clution--current-clution*
    (clution-close))

  (let ((path (expand-file-name path)))
    (setf *clution--current-clution*
          (clution--parse-file path))
    (setf *clution--current-watch*
          (file-notify-add-watch path '(change) 'clution--file-watch-callback))

    (with-current-buffer (clution--output-buffer)
      (setf default-directory (clution--clution.dir *clution--current-clution*))))
  (run-hooks 'clution-open-hook))

(defun clution-close ()
  (interactive)
  (when *clution--current-clution*
    (file-notify-rm-watch *clution--current-watch*)
    (setf *clution--current-watch* nil)
    (setf *clution--current-clution* nil)

    (run-hooks 'clution-close-hook)))

(add-to-list 'auto-mode-alist '("\\.clu$" . clution-file-mode))
(add-hook 'find-file-hook 'clution--find-file-hook)

(add-to-list 'purpose-user-mode-purposes '(clution-output-mode . clution-output))
(purpose-compile-user-configuration)

(provide 'clution)
