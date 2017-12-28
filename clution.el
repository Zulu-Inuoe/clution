(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))

(defun clution--eval-in-lisp (sexpr)
  (lexical-let* ((lisp-proc nil)
                 (output ""))
    (unwind-protect
        (progn
          (setf
           lisp-proc
           (make-process
            :name "*clution-lisp-eval*"
            :command (list* (clution--spawn-lisp-command) (clution--spawn-lisp-args))
            :filter
            (lambda (proc string)
              (setf output (concat output string)))))
          (process-send-string
           lisp-proc
           (format "%S\n"
                   `(cl:progn
                     (cl:prin1
                      (cl:let ((cl:*standard-input* (cl:make-broadcast-stream))
                               (cl:*error-output* (cl:make-broadcast-stream))
                               (cl:*standard-output* (cl:make-broadcast-stream))
                               (cl:*trace-output* (cl:make-broadcast-stream))
                               (cl:*debug-io* (cl:make-broadcast-stream))
                               (cl:*query-io* (cl:make-broadcast-stream)))
                              (cl:handler-case (cl:cons :success ,sexpr)
                                               (cl:error (err)
                                                         (cl:cons :fail (cl:princ-to-string err))))))
                     (cl:finish-output)
                     ,(clution--exit-form 0))))
          (while (process-live-p lisp-proc)
            (accept-process-output lisp-proc))
          (let ((res (car (read-from-string output))))
            (unless (eq (car res) :SUCCESS)
              (error "clution: error during eval: %S" (cdr res)))
            (cdr res)))
      (when lisp-proc
        (delete-process lisp-proc)))))

(defun clution--systems-query (systems)
  (let ((names-paths-alist
         (mapcar
          (lambda (system)
            (cons
             (clution--system.name system)
             (clution--system.path system)))
          systems)))
    (clution--eval-in-lisp
     `(cl:labels ((translate-component (component type)
                                       (cl:list
                                        :name (asdf:component-name component)
                                        :path (cl:namestring (asdf:component-pathname component))
                                        :type type))
                  (recurse (component)
                           (cl:etypecase component
                                         (asdf:parent-component
                                          (cl:cons
                                           (translate-component component :parent)
                                           (cl:mapcar #'recurse (asdf:component-children component))))
                                         (asdf:child-component
                                          (cons (translate-component component :child) nil)))))
                 (cl:loop
                  :for (name . path) :in ',names-paths-alist
                  :do (asdf:load-asd (cl:parse-namestring path))
                  :collect (recurse (asdf:find-system name)))))))

(defun clution--system-query (system)
  (car (clution--systems-query (list system))))

(defun clution--add-system (clution path)
  (let ((system (clution--make-system
                 (list :path path))))
    (clution--clution.add-system clution system))
  (clution--save-clution clution))

(defun clution--remove-system (system &optional delete)
  (let ((clution (clution--system.clution system)))
    (unless clution
      (error "system has no parent clution"))
    (clution--clution.remove-system clution system)
    (clution--save-clution clution)))

(defvar *clution--current-clution* nil)
(defvar *clution--current-watch* nil)
(defvar *clution--repl-active* nil)
(defvar *clution--current-op* nil)

;;; Buffer and window manipulation
(defvar *clution--output-window* nil)
(defvar *clution--repl-window* nil)
(defvar *clution--clutex-window* nil)

(defun clution--insert-clution-button (clution)
  (lexical-let* ((clution clution)
                 (map (make-sparse-keymap))
                 (button
                  (insert-button
                   (clution--clution.name clution)
                   'keymap map)))
    (define-key map (kbd "C-m")
      (lambda ()
        (interactive)
        (if-let ((path (clution--clution.path clution)))
            (find-file path)
          (message "clution is virtual"))))
    (define-key map (kbd "<double-down-mouse-1>") (kbd "C-m"))

    (define-key map (kbd "A")
      (lambda ()
        (interactive)
        (let ((path (clution--read-file-name "Add system:" (clution--clution.dir clution) nil t)))
          (clution--add-system clution path))))
    button))

(defun clution--insert-system-button (system)
  (lexical-let* ((system system)
                 (map (make-sparse-keymap))
                 (button
                  (insert-button
                   (concat "> " (file-name-nondirectory (clution--system.path system)))
                   'keymap map)))
    (define-key map (kbd "TAB")
      (lambda ()
        (interactive)
        (clution--toggle-system-fold system)))
    (define-key map (kbd "<mouse-1>")
      (lambda ()
        (interactive)
        (clution--toggle-system-fold system)))
    (define-key map (kbd "<delete>")
      (lambda ()
        (interactive)
        (clution--remove-system system nil)))
    (define-key map (kbd "D") (kbd "<delete>"))
    (define-key map (kbd "S-<delete>")
      (lambda ()
        (interactive)
        (clution--remove-system system t)))
    (define-key map (kbd "C-m")
      (lambda ()
        (interactive)
        (find-file (clution--system.path system))))
    (define-key map (kbd "<double-down-mouse-1>") (kbd "C-m"))
    button))

(defun clution--insert-parent-button (system parent)
  (lexical-let* ((system system)
                 (parent parent)
                 (map (make-sparse-keymap))
                 (button
                  (insert-button
                   (concat "> " (getf (car parent) :NAME))
                   'keymap map)))
    button))

(defun clution--insert-child-button (system child)
  (lexical-let* ((system system)
                 (child child)
                 (map (make-sparse-keymap))
                 (button
                  (insert-button
                   (file-name-nondirectory (getf (car child) :PATH))
                   'keymap map)))
    (define-key map (kbd "C-m")
      (lambda ()
        (interactive)
        (find-file (getf (car child) :PATH))))
    (define-key map (kbd "<double-down-mouse-1>") (kbd "C-m"))
    (define-key map (kbd "<delete>")
      (lambda ()
        (interactive)
        (clution--remove-child child nil)))
    (define-key map (kbd "S-<delete>")
      (lambda ()
        (interactive)
        (clution--remove-child child t)))
    button))

(defun clution--insert-nodes (system nodes indent)
  (dolist (node nodes)
    (insert-char ?\s indent)
    (cl-case (getf (car node) :TYPE)
      (:CHILD
       (clution--insert-child-button system node)
       (insert "\n"))
      (:PARENT
       (clution--insert-parent-button system node)
       (insert "\n")
       (clution--insert-nodes system (cdr node) (+ indent 2))))))

(defun clution--populate-clutex (clution buffer)
  (with-current-buffer buffer
    (clution--insert-clution-button clution)
    (insert "\n")
    (dolist (system (clution--clution.systems clution))
      (insert "  ")
      (clution--insert-system-button system)
      (insert "\n")
      (clution--insert-nodes system (clution--system.children system) 4))))

(defun clution--output-buffer (&optional create)
  (let ((buffer (get-buffer "*clution-output*")))
    (when (and (null buffer) create)
      (setf buffer (generate-new-buffer "*clution-output*"))
      (with-current-buffer buffer
        (clution-output-mode)))
    buffer))

(defun clution--clutex-buffer (&optional create)
  (let ((buffer (get-buffer "*clution-clutex*")))
    (when (and (null buffer) create)
      (setf buffer (generate-new-buffer "*clution-clutex*"))
      (with-current-buffer buffer
        (clutex-mode)))
    buffer))

(defun clution--read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  (let ((path (read-file-name prompt dir default-filename mustmatch initial predicate)))
    (while (directory-name-p path)
      (setq path (read-file-name prompt path default-filename mustmatch initial predicate)))
    path))

(defun clution--read-new-file-name (prompt &optional dir default-filename initial predicate)
  (let ((path (read-file-name prompt dir default-filename nil initial predicate)))
    (while (or (file-exists-p path)
               (directory-name-p path))
      (setq path
            (read-file-name prompt (file-name-directory path) default-filename nil initial predicate)))
    path))

(defun clution--temp-dir ()
  (file-name-as-directory
   (expand-file-name
    "clution"
    temporary-file-directory)))

(defun clution--temp-asd-clution-dir ()
  (file-name-as-directory
   (expand-file-name
    "asd-clution"
    (clution--temp-dir))))

(defun clution--set-window-height (window n)
  "Make WINDOW N rows height."
  (with-selected-window window
    (let ((h (max n window-min-height)))
      (unless (null window)
        (if (> (window-height) h)
            (shrink-window (- (window-height) h))
          (if (< (window-height) h)
              (enlarge-window (- h (window-height)))))))))

(defun clution--set-window-width (window n)
  "Make WINDOW N columns width."
  (with-selected-window window
    (let ((w (max n window-min-width)))
      (unless (null window)
        (if (> (window-width) w)
            (shrink-window-horizontally (- (window-width) w))
          (if (< (window-width) w)
              (enlarge-window-horizontally (- w (window-width)))))))))

(defun clution--kill-buffer-if-no-window (buffer-or-name)
  (when-let ((buffer (get-buffer buffer-or-name)))
    (unless (get-buffer-window-list buffer)
      (kill-buffer buffer))))

(defun clution--sync-output (clution buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cond
       (clution
        (setf default-directory (clution--clution.dir clution))
        (insert "Output buffer for '" (clution--clution.name clution) "' (" (clution--clution.dir clution) ")\n"))
       (t
        (insert "No clution open.\n"))))))

(defun clution--sync-clutex (clution buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (point (point)))
      (erase-buffer)
      (cond
       (clution
        (setf default-directory (clution--clution.dir clution))
        (clution--populate-clutex clution buffer)
        (goto-char point))
       (t
        (insert "No clution open.\n"))))))

(defun clution--sync-buffers (clution)
  (when-let ((buffer (clution--output-buffer)))
    (clution--sync-output clution buffer)
    (clution--kill-buffer-if-no-window buffer))

  (when-let ((buffer (clution--clutex-buffer)))
    (clution--sync-clutex clution buffer)
    (clution--kill-buffer-if-no-window buffer)))

(defun clution--init-output-window (window)
  (set-window-dedicated-p window t)
  (clution--set-window-height window 15))

(defun clution--init-clutex-window (window)
  (set-window-dedicated-p window t)
  (clution--set-window-width window clution-clutex-width))

;;;; Clution/System data structures, and loaders

(defun clution--update-system-query (system)
  (when-let ((query (ignore-errors (clution--system-query system))))
    (setf (getf system :system-query) query)))

(defun clution--insert-system (system indent)
  (let ((clution (clution--system.clution system)))
    (insert "(:path \""
            (file-relative-name
             (clution--system.path system)
             (clution--clution.dir clution))
            "\"")
    (when-let ((startup-dir (getf system :startup-dir)))
      (insert " :startup-dir " (format "%S" startup-dir)))
    (when-let ((toplevel (getf system :toplevel)))
      (insert " :toplevel " (format "%S" toplevel)))
    (when-let ((args (getf system :args)))
      (insert " :args " (format "%S" args)))
    (insert ")")))

(defun clution--make-system (data &optional clution)
  (unless (getf data :path)
    (error "clution: system missing :path component: %S" data))
  (let ((res
         (list
          :path
          (expand-file-name
           (getf data :path)
           (when clution (clution--clution.dir clution)))
          :clution clution
          :startup-dir
          (when-let ((dir (getf data :startup-dir)))
            (file-name-as-directory
             (expand-file-name
              dir
              (when clution (clution--clution.dir clution)))))
          :toplevel (getf data :toplevel)
          :args (getf data :args)
          :system-query nil)))
    (unless clution
      (setf (getf res :system-query) (clution--system-query res)))
    res))

(defun clution--make-cuo (data)
  (copy-list data))

(defun clution--parse-cuo-file (path)
  (clution--make-cuo
   (car
    (read-from-string
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string))))))

(defun clution--insert-clution (clution indent)
  (insert "(")
  (let ((first t))
    (when-let ((out-dir (getf clution :output-dir)))
      (if first
          (setq first nil)
        (insert-char ?\s (1+ indent)))
      (insert ":output-dir" (format "%S" out-dir) "\n"))
    (when-let ((tmp-dir (getf clution :tmp-dir)))
      (if first
          (setq first nil)
        (insert-char ?\s (1+ indent)))
      (insert ":tmp-dir" (format "%S" tmp-dir) "\n"))
    (if first
        (setq first nil)
      (insert-char ?\s (1+ indent)))
    (insert ":systems")
    (let ((systems (clution--clution.systems clution)))
      (if systems
          (progn
            (insert "\n")
            (insert-char ?\s (1+ indent))
            (insert "(")
            (while systems
              (clution--insert-system (car systems) (+ indent 2))
              (setq systems (cdr systems))
              (when systems
                (insert "\n")
                (insert-char ?\s (+ indent 2))))
            (insert ")"))
        (insert " ()"))))
  (insert ")\n"))

(defun clution--save-clution (&optional clution path)
  (unless clution
    (setf clution *clution--current-clution*))
  (unless path
    (setq path (clution--clution.path clution)))
  (when path
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (clution--insert-clution clution 0))))

(defun clution--make-clution (data &optional path)
  (let ((res
         (list
          :path path
          :systems nil
          :output-dir
          (when-let ((dir (getf data :output-dir)))
            (file-name-as-directory dir))
          :tmp-dir
          (when-let ((dir (getf data :tmp-dir)))
            (file-name-as-directory dir))
          :cuo nil)))

    (setf (getf res :systems)
          (mapcar
           (lambda (sys-data)
             (clution--make-system sys-data res))
           (getf data :systems)))

    (let ((queries (clution--systems-query (clution--clution.systems res))))
      (dolist (sys (clution--clution.systems res))
        (setf (getf sys :system-query) (car queries))
        (pop queries)))

    (when (file-exists-p (clution--clution.cuo-path res))
      (setf (getf res :cuo) (clution--parse-cuo-file (clution--clution.cuo-path res))))

    res))

(defun clution--make-asd-clution (asd-path)
  (let ((asd-clution-path
         (expand-file-name
          (concat (file-name-base asd-path) ".clu")
          (expand-file-name
           (file-name-base asd-path)
           (clution--temp-asd-clution-dir)))))
    (let ((res
           (list
            :path asd-clution-path
            :systems nil
            :output-dir nil
            :tmp-dir nil
            :cuo nil)))

      (let ((sys (clution--make-system
                  (list :path asd-path)
                  res)))
        (setf (getf res :systems) (list sys))
        (setf (getf sys :system-query) (clution--system-query sys)))
      res)))

(defun clution--parse-file (path)
  (clution--make-clution
   (car
    (read-from-string
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string))))
   path))

(defun clution--clution.name (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (downcase (file-name-base (clution--clution.path clution))))

(defun clution--clution.path (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (getf clution :path))

(defun clution--clution.systems (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (getf clution :systems))

(defun clution--clution.add-system (clution system)
  (setf (getf clution :systems)
        (nconc (getf clution :systems) (list system))))

(defun clution--clution.remove-system (clution system)
  (setf (getf clution :systems)
        (delete system (getf clution :systems))))

(defun clution--clution.cuo (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (getf clution :cuo))

(defun clution--clution.selected-system (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (find
   (or (getf (clution--clution.cuo clution) :selected-system)
       (let ((sys (first (clution--clution.systems clution))))
         (if sys (clution--system.name sys) nil)))
   (clution--clution.systems clution)
   :key 'clution--system.name
   :test 'string-equal))

(defun clution--clution.output-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (or (getf clution :output-dir)
      (file-name-as-directory
       (expand-file-name
        "out"
        (clution--clution.dir clution)))))

(defun clution--clution.tmp-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (or (getf clution :tmp-dir)
      (file-name-as-directory
       (expand-file-name
        "tmp"
        (clution--clution.dir clution)))))

(defun clution--clution.cuo-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (expand-file-name
    "cuo"
    (clution--clution.tmp-dir clution))))

(defun clution--clution.cuo-path (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (expand-file-name
   (concat (clution--clution.name clution) ".cuo")
   (clution--clution.cuo-dir clution)))

(defun clution--clution.script-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (expand-file-name
    "script"
    (clution--clution.tmp-dir clution))))

(defun clution--clution.asdf-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (expand-file-name
    "asdf"
    (clution--clution.tmp-dir clution))))

(defun clution--clution.dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-directory (clution--clution.path clution)))

(defun clution--system.path (clution-system)
  (getf clution-system :path))

(defun clution--system.dir (clution-system)
  (file-name-directory (clution--system.path clution-system)))

(defun clution--system.query (clution-system)
  (getf clution-system :system-query))

(defun clution--system.children (clution-system)
  (cdr (clution--system.query clution-system)))

(defun clution--system.query-prop (clution-system prop)
  (getf (car (clution--system.query clution-system)) prop))

(defun clution--system.name (clution-system)
  (or (clution--system.query-prop clution-system :NAME)
      (downcase (file-name-base (clution--system.path clution-system)))))

(defun clution--system.clution (clution-system)
  (getf clution-system :clution))

(defun clution--system.toplevel (clution-system)
  (or (getf clution-system :toplevel)
      "common-lisp-user::main"))

(defun clution--system.startup-dir (clution-system)
  (or (getf clution-system :startup-dir)
      (clution--system.dir clution-system)))

(defun clution--system.type (clution-system)
  (or (getf clution-system :type)
      :library))

(defun clution--system.args (clution-system)
  (getf clution-system :args))

(defun clution--system.cache-dir (clution-system)
  (or (and (clution--system.clution clution-system)
           (file-name-as-directory
            (expand-file-name
             (clution--system.name clution-system)
             (clution--clution.asdf-dir (clution--system.clution clution-system)))))
      (error "no cache dir for %S" clution-system)))

(defun clution--system.script-path (clution-system)
  (concat
   (file-name-as-directory
    (concat
     (clution--clution.script-dir (clution--system.clution clution-system))
     (clution--system.name clution-system)))
   (clution--system.name clution-system)
   "-script.lisp"))

;;;; Frontend/Backend Lisp access

(defun clution--spawn-lisp-command ()
  "Command to spawn a lisp in a REL (repl without the print)."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl "sbcl")))
    (roswell "ros")))

(defun clution--spawn-lisp-args ()
  "Arguments to spawn a lisp in a REL (repl without the print)."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl
        '("--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
          "--noprint" "--disable-debugger"))))
    (roswell
     (cl-ecase clution-backend
       (sbcl
        '("run" "--lisp" "sbcl-bin" "--"
          "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
          "--noprint" "--disable-debugger"))))))

(defun clution--spawn-script-command ()
  "Command to spawn a lisp which will load a script file, then exit."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl "sbcl")))
    (roswell "ros")))

(defun clution--spawn-script-args (system)
  "Arguments to spawn a lisp which will load a script file, then exit."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl
        `("--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
          "--noprint" "--disable-debugger" "--load" ,(clution--system.script-path system) "--eval" "(sb-ext:exit :code 0)"))))
    (roswell
     (cl-ecase clution-backend
       (sbcl
        `("run" "--lisp" "sbcl-bin" "--eval" ,(format "%S" (clution--run-form system)) "-q"))))))

(defun clution--spawn-repl-command ()
  "Command to spawn a lisp in a REPL."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl "sbcl")))
    (roswell "ros")))

(defun clution--spawn-repl-args ()
  "Arguments to spawn a lisp in a REPL."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl `())))
    (roswell
     (cl-ecase clution-backend
       (sbcl '("run" "--lisp" "sbcl-bin"))))))

(defun clution--args-list-form ()
  "An SEXP which when evaluated in the lisp backend will evaluate to the list
of command-line arguments"
  (cl-ecase clution-backend
    (t
     'uiop:*command-line-arguments*)))

(defun clution--exit-form (exit-code-form)
  "A SEXP which when evaluated in the lisp backend, will exit the program with
the code obtained from evaluating the given `exit-code-form'."
  (cl-ecase clution-backend
    (t
     `(uiop:quit ,exit-code-form cl:t))))

(defun clution--with-system-searcher (clution lispexpr)
  (let ((names-paths-alist
         (mapcar
          (lambda (system)
            (cons
             (clution--system.name system)
             (clution--system.path system)))
          (clution--clution.systems clution)))
        (system-output-translations
         (mapcar
          (lambda (system)
            (list
             (expand-file-name
              "**/*.*"
              (clution--system.dir system))
             (expand-file-name
              "**/*.*"
              (clution--system.cache-dir system))))
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
        (toplevel (clution--system.toplevel system))
        (args (clution--system.args system)))
    (clution--with-system-searcher (clution--system.clution system)
     `(progn
        (let ((*standard-output* (make-broadcast-stream))
              (*trace-output* (make-broadcast-stream)))
          (asdf:load-system ,system-name :verbose nil))

        (handler-case
            (let ((ret-code (apply (read-from-string ,toplevel) ',args)))
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
  (cl-ecase clution-run-style
    (comint
     (let ((clution-run-buffer (get-buffer-create "*clution-run-buffer*")))
       (with-current-buffer clution-run-buffer
         (setq default-directory (clution--system.startup-dir system))
         (setq buffer-read-only nil)
         (erase-buffer)
         (comint-mode))
       (make-process
        :name "clution-spawn-script"
        :buffer clution-run-buffer
        :command (list* (clution--spawn-script-command) (clution--spawn-script-args system))
        :connection-type nil
        :noquery nil
        :sentinel
        (lexical-let ((buffer clution-run-buffer)
                      (sentinel sentinel))
          (lambda (proc event)
            (funcall sentinel proc event)
            (when (eq (process-status proc) 'exit)
              (with-current-buffer buffer
                (let ((status (process-exit-status proc)))
                  (insert "\n\nFinished running. Exited with code "
                          (number-to-string status)
                          "(0x" (format "%x" status) ")\n\n"))
                (setq buffer-read-only t))))))
       (select-window
        (display-buffer-in-side-window clution-run-buffer '((side . bottom)
                                                            (slot . -1))))))
    (term
     (cl-ecase system-type
       (windows-nt
        (let ((default-directory (clution--system.startup-dir system)))
          (let ((proc
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
            (set-process-sentinel proc sentinel))))))))

(defun clution--clear-output ()
  (when-let ((buffer (clution--output-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun clution--append-output (&rest args)
  (when-let ((buffer (clution--output-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (apply 'insert args)))
    (when-let ((output-window (get-buffer-window buffer)))
      (with-selected-window output-window
        (set-window-point output-window (point-max))))))

(defvar *clution--build-remaining-systems* nil)

(defun clution--build-filter (proc string)
  (clution--append-output string))

(defun clution--build-sentinel (proc event)
  (cl-case (process-status proc)
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
                           (format "%S\n" (clution--build-form system t))))))

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
            (clution--append-output (with-current-buffer (sly-buffer-name :compilation) (buffer-string)) "\n\n")
            (clution--build-complete))))
      "COMMON-LISP-USER")))

(defun clution--repl-slime-compile (systems)
  (let* ((clution (clution--system.clution (car systems)))
         (system-names (mapcar 'clution--system.name systems)))
    (slime-eval-async
        `(cl:progn
          ,(clution--with-system-searcher clution
                                          `(swank::collect-notes
                                            (cl:lambda ()
                                                       (cl:dolist (system-name ',system-names)
                                                                  (cl:handler-case
                                                                   (swank::with-compilation-hooks ()
                                                                                                  (asdf:compile-system system-name :force t))
                                                                   (asdf:compile-error ()
                                                                                       nil)
                                                                   (asdf/lisp-build:compile-file-error ()
                                                                                                       nil)))))))
      (lexical-let ((clution clution))
        (lambda (result)
          (let ((default-directory (clution--clution.dir clution)))
            (slime-compilation-finished result)
            (clution--append-output (with-current-buffer (slime-buffer-name :compilation) (buffer-string)) "\n\n")
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
    (cl-ecase clution-repl-style
      (sly
       (clution--repl-sly-compile systems))
      (slime
       (clution--repl-slime-compile systems)))))

(defun clution--build-complete ()
  (clution--append-output
   "Build Complete: '" (clution--clution.name) "'\n")

  (setf *clution--current-op* nil)
  (run-hooks 'clution-build-complete-hook))

(defun clution--run-sentinel (proc event)
  (cl-case (process-status proc)
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
    (let ((success nil))
      (unwind-protect
          (progn
            (clution--spawn-script system 'clution--run-sentinel)
            (setf success t))
        (unless success
          (setq *clution--current-op* nil))))))

(defun clution--do-clean (systems)
  (dolist (system systems)
    (let ((system-cache-dir (clution--system.cache-dir system)))
      (when (file-exists-p system-cache-dir)
        (clution--append-output
         "Removing '" (clution--system.name system)
         "' asdf cache: '" system-cache-dir
         "'\n\n")

        (delete-directory system-cache-dir t)))))

(defun clution--display-in-repl-window (buffer)
  (unless (window-live-p *clution--repl-window*)
    (setf *clution--repl-window* (display-buffer buffer '(clution-repl-default-display-fn))))
  (when (window-live-p *clution--repl-window*)
    (select-window *clution--repl-window*)
    (set-window-dedicated-p nil nil)
    (set-window-buffer nil buffer)
    (set-window-dedicated-p nil t)
    (goto-char (point-max))
    (selected-window)))

(defun clution--sly-mrepl-on-connection-advice (orig-fun)
  (advice-remove 'sly-mrepl-on-connection 'clution--sly-mrepl-on-connection-advice)
  (save-window-excursion
    (funcall orig-fun))
  (clution--display-in-repl-window (sly-mrepl)))

(defun clution--start-sly-repl ()
  (when (and clution-intrusive-ui
             (featurep 'sly-mrepl))
    (advice-add 'sly-mrepl-on-connection :around 'clution--sly-mrepl-on-connection-advice))

  ;;Set up hooks for successful connect and failed start
  (lexical-let (connected-hook start-failed-hook)
    ;;Set up hook for when we successfully connect to SLIME
    (setf connected-hook
          (lambda ()
            (remove-hook 'clution-repl-start-failed-hook start-failed-hook)
            (remove-hook 'sly-connected-hook connected-hook)

            (when (and clution-intrusive-ui
                       (not (featurep 'sly-mrepl))
                       (not clution-show-inferior-start))
              ;;Display inferior lisp if we aren't already
              (clution--display-in-repl-window (process-buffer (sly-inferior-process))))

            ;;Set up hook to detect slime repl disconnecting
            (lexical-let (net-close-hook)
              (setf net-close-hook
                    (lambda (proc)
                      (remove-hook 'sly-net-process-close-hooks net-close-hook)
                      (clution--repl-exited)))
              (add-hook 'sly-net-process-close-hooks net-close-hook))

            (clution--repl-started)))
    (add-hook 'sly-connected-hook connected-hook)

    ;;Set up hook for when starting fails
    (setf start-failed-hook
          (lambda ()
            (when (and clution-intrusive-ui
                       (featurep 'sly-mrepl))
              (advice-remove 'sly-mrepl-on-connection 'clution--sly-mrepl-on-connection-advice))
            (remove-hook 'clution-repl-start-failed-hook start-failed-hook)
            (remove-hook 'sly-connected-hook connected-hook)))
    (add-hook 'clution-repl-start-failed-hook start-failed-hook))

  (let ((sly-inferior-buffer
         (sly-start
          :program (clution--spawn-repl-command)
          :program-args (clution--spawn-repl-args)
          :directory (clution--clution.dir)
          :init (lambda (port-filename coding-system)
                  (format "(progn %s %S)\n\n"
                          (funcall sly-init-function port-filename coding-system)
                          (clution--repl-form *clution--current-clution*))))))

    (lexical-let* ((sly-inferior-process (get-buffer-process sly-inferior-buffer))
                   (prev-sentinel (process-sentinel sly-inferior-process))
                   connected-hook)

      ;;Set up a hook for when we successfully connect, since we won't need to detect
      ;;a failed startup any more
      (setq connected-hook
            (lambda ()
              ;;Remove the hook
              (remove-hook 'sly-connected-hook connected-hook)
              ;;Restore the previous sentinel
              (set-process-sentinel sly-inferior-process prev-sentinel)))

      (add-hook 'sly-connected-hook connected-hook)

      ;;Install a sentinel for when inferior lisp dies before sly is set up
      ;;This way we can detect a failed start
      (set-process-sentinel sly-inferior-process
                            (lambda (proc event)
                              (cl-case (process-status proc)
                                (exit
                                 ;;Remove the hook
                                 (remove-hook 'sly-connected-hook connected-hook)
                                 ;;Restore the previous sentinel
                                 (set-process-sentinel sly-inferior-process prev-sentinel)
                                 ;;Notify that it failed to start
                                 (clution--repl-start-failed)))
                              (funcall prev-sentinel proc event))))

    (when (and clution-intrusive-ui
               clution-show-inferior-start)
      (clution--display-in-repl-window sly-inferior-buffer))))

(defun clution--slime-repl-connected-hook-function-advice (orig-fun)
  (advice-remove 'slime-repl-connected-hook-function 'clution--slime-repl-connected-hook-function-advice)
  (when (window-live-p *clution--repl-window*)
    (set-window-dedicated-p *clution--repl-window* nil))
  (save-window-excursion
    (funcall orig-fun))
  (clution--display-in-repl-window (slime-repl-buffer)))

(defun clution--start-slime-repl ()
  ;;Set up advice for switching to the slime repl after connection
  (when (and clution-intrusive-ui
             (featurep 'slime-repl))
    (advice-add 'slime-repl-connected-hook-function :around 'clution--slime-repl-connected-hook-function-advice))

  ;;Set up hooks for successful connect and failed start
  (lexical-let (connected-hook start-failed-hook)
    ;;Set up hook for when we successfully connect to SLIME
    (setf connected-hook
          (lambda ()
            (remove-hook 'clution-repl-start-failed-hook start-failed-hook)
            (remove-hook 'slime-connected-hook connected-hook)

            (when (and clution-intrusive-ui
                       (not (featurep 'slime-repl))
                       (not clution-show-inferior-start))
              ;;Display inferior lisp if we aren't already
              (clution--display-in-repl-window (process-buffer (slime-inferior-process))))

            ;;Set up hook to detect slime repl disconnecting
            (lexical-let (net-close-hook)
              (setf net-close-hook
                    (lambda (proc)
                      (remove-hook 'slime-net-process-close-hooks net-close-hook)
                      (clution--repl-exited)))
              (add-hook 'slime-net-process-close-hooks net-close-hook))

            (clution--repl-started)))
    (add-hook 'slime-connected-hook connected-hook t)

    ;;Set up hook for when starting fails
    (setf start-failed-hook
          (lambda ()
            (when (and clution-intrusive-ui
                       (featurep 'slime-repl))
              (advice-remove 'slime-repl-connected-hook-function 'clution--slime-repl-connected-hook-function-advice))
            (remove-hook 'clution-repl-start-failed-hook start-failed-hook)
            (remove-hook 'slime-connected-hook connected-hook)))
    (add-hook 'clution-repl-start-failed-hook start-failed-hook))

  (let ((slime-inferior-buffer
         (cl-flet ((do-start ()
                             (slime-start
                              :program (clution--spawn-repl-command)
                              :program-args (clution--spawn-repl-args)
                              :directory (clution--clution.dir)
                              :init (lambda (port-filename coding-system)
                                      (format "(progn %s %S)\n\n"
                                              (slime-init-command port-filename coding-system)
                                              (clution--repl-form *clution--current-clution*))))))
           (if clution-intrusive-ui
               (save-window-excursion
                 (do-start))
             (do-start)))))
    (lexical-let* ((slime-inferior-process (get-buffer-process slime-inferior-buffer))
                   (prev-sentinel (process-sentinel slime-inferior-process))
                   connected-hook)

      ;;Set up a hook for when we successfully connect, since we won't need to detect
      ;;a failed startup any more
      (setq connected-hook
            (lambda ()
              ;;Remove the hook
              (remove-hook 'slime-connected-hook connected-hook)
              ;;Restore the previous sentinel
              (message "Old sentinel vs new: %s %s" prev-sentinel (process-sentinel slime-inferior-process))
              (set-process-sentinel slime-inferior-process prev-sentinel)))

      (add-hook 'slime-connected-hook connected-hook)

      ;;Install a sentinel for when inferior lisp dies before sly is set up
      ;;This way we can detect a failed start
      (set-process-sentinel slime-inferior-process
                            (lambda (proc event)
                              (cl-case (process-status proc)
                                (exit
                                 ;;Remove the hook
                                 (remove-hook 'slime-connected-hook connected-hook)
                                 ;;Restore the previous sentinel
                                 (set-process-sentinel slime-inferior-process prev-sentinel)
                                 ;;Notify that it failed to start
                                 (clution--repl-start-failed)))
                              (funcall prev-sentinel proc event))))

    ;;If we want to show the inferior buffer during startup, do so
    (when (and clution-intrusive-ui
               clution-show-inferior-start)
      (clution--display-in-repl-window slime-inferior-buffer))))

(defun clution--start-repl ()
  (clution--append-output
   "\nclution-repl: starting with style '" (format "%s" clution-repl-style) "'\n")
  (setf *clution--current-op*
        (list
         :type 'clution-start-repl))

  (cl-ecase clution-repl-style
    (sly
     (clution--start-sly-repl))
    (slime
     (clution--start-slime-repl))))

(defun clution--restart-repl ()
  (cl-ecase clution-repl-style
    (sly
     (when (sly-connected-p)
       (sly-quit-lisp-internal
        (sly-connection)
        (lambda (proc message)
          (funcall 'sly-quit-sentinel proc message)
          (clution--start-repl))
        t)))
    (slime
     (when (slime-connected-p)
       (lexical-let ((sentinel
                      (lambda (proc message)
                        (cl-assert (process-status proc) 'closed)
                        (let* ((inferior (slime-inferior-process proc)))
                          (when inferior (delete-process inferior))
                          (slime-net-close proc))
                        (clution--start-repl))))
         (slime-quit-lisp-internal (slime-connection) sentinel t))))))

(defun clution--end-repl ()
  (cl-ecase clution-repl-style
    (sly
     (when (sly-connected-p)
       (sly-quit-lisp t)))
    (slime
     (when (slime-connected-p)
       (lexical-let ((sentinel
                      (lambda (proc message)
                        (cl-assert (process-status proc) 'closed)
                        (let* ((inferior (slime-inferior-process proc)))
                          (when inferior (delete-process inferior))
                          (slime-net-close proc))

                        (when (featurep 'slime-repl)
                          (slime-kill-all-buffers)))))
         (slime-quit-lisp-internal (slime-connection) sentinel t)))))

  (when clution-intrusive-ui
    (when (window-live-p *clution--repl-window*)
      (delete-window *clution--repl-window*))
    (setf *clution--repl-window* nil)))

(defun clution--repl-sentinel (proc event)
  (cl-case (process-status proc)
    (exit
     (clution--repl-exited))))

(defun clution--repl-start-failed ()
  (clution--append-output "\nclution-repl: repl failed to start\n")
  (when clution-intrusive-ui
    (when (window-live-p *clution--repl-window*)
      (delete-window *clution--repl-window*))
    (setf *clution--repl-window* nil))

  (run-hooks 'clution-repl-start-failed-hook)
  (setf *clution--current-op* nil))

(defun clution--repl-started ()
  (clution--append-output "\nclution-repl: repl started\n")
  (run-hooks 'clution-repl-started-hook)
  (setf *clution--current-op* nil)
  (setf *clution--repl-active* t))

(defun clution--repl-exited ()
  (clution--append-output "\nclution-repl: repl exited\n")
  (when clution-intrusive-ui
    (when (window-live-p *clution--repl-window*)
      (delete-window *clution--repl-window*))
    (setf *clution--repl-window* nil))
  (run-hooks 'clution-repl-exited-hook)
  (setf *clution--repl-active* nil))

(defun clution--find-file-hook ()
  (let ((path (buffer-file-name)))
    (cond
     ((not clution-auto-open)
      nil)
     (*clution--current-clution*
      nil)
     ((not (file-exists-p path))
      nil)
     ((null (file-name-extension path))
      nil)
     ((string-match-p "^clu$" (file-name-extension path))
      (clution-open path))
     ((string-match-p "^asd$" (file-name-extension path))
      (clution-open-asd path)))))

(defun clution--file-watch-callback (event)
  (cl-destructuring-bind (descriptor action file &optional file1)
      event
    (cl-case action
      (created)
      (deleted
       (clution-close))
      (changed
       (message "Reloading clution (changed %S)" file)
       (setf *clution--current-clution* (clution--parse-file file))
       (clution--sync-buffers *clution--current-clution*))
      (renamed
       (message "Reloading clution (rename to %S)" file1)
       (setf *clution--current-clution* (clution--parse-file file1))
       (clution--sync-buffers *clution--current-clution*))
      (attribute-changed)
      (stopped))))

;;;; Publix interface

;;; Customization

(defgroup clution nil
  "Options for clution."
  :prefix "clution-")

(defcustom clution-frontend 'roswell
  "The frontend to use as default for clution."
  :type '(choice (const :tag "Use clution-backend directly" raw)
                 (const :tag "Roswell" roswell))
  :group 'clution)

(defcustom clution-backend 'sbcl
  "The backend to use as default for clution."
  :type '(choice (const :tag "sbcl" sbcl))
  :group 'clution)

(defcustom clution-run-style 'comint
  "How to 'run' a clution."
  :type '(choice (const :tag "Run process inside emacs window via comint" comint)
                 (const :tag "Run process in external terminal" term))
  :group 'clution)

(defcustom clution-repl-style (if (package-installed-p 'sly) 'sly 'slime)
  "The type of repl to use for clution"
  :type '(choice (const :tag "Use Sly" sly)
                 (const :tag "Use SLIME" slime))
  :group 'clution)

(defcustom clution-show-inferior-start 't
  "When enabled, the inferior-lisp startup buffer used by Sly/SLIME will be placed in the repl window during startup.
This only matters when `clution-intrusive-ui' is enabled."
  :type 'boolean
  :group 'clution)

(defcustom clution-auto-open 't
  "When enabled, clution will automatically open when visiting a .clu or .asd file."
  :type 'boolean
  :group 'clution)

(defcustom clution-intrusive-ui 't
  "When enabled, clution will automatically open the clutex and output buffers when opening a clution."
  :type 'boolean
  :group 'clution)

(defgroup clutex nil
  "Options for clutex."
  :prefix "clution-clutex-"
  :group 'clution)

(defcustom clution-clutex-position 'right
  "The position of the clutex window."
  :type '(choice (const right)
                 (const left))
  :group 'clutex)

(defcustom clution-clutex-width 25
  "The width of the clutex window."
  :type 'integer
  :group 'clutex)

;;; Hooks
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

(defvar clution-repl-start-failed-hook nil
  "Hook executed whenever a 'repl' operation fails to start.")

(defvar clution-repl-started-hook nil
  "Hook executed whenever a 'repl' operation starts.")

(defvar clution-repl-exited-hook nil
  "Hook executed whenever a 'repl' operation exits.")

;;; Modes and maps

(defvar clution-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-S-b") 'clution-build)
    (define-key map (kbd "<f5>") 'clution-repl)
    (define-key map (kbd "S-<f5>") 'clution-end-repl)
    (define-key map (kbd "C-<f5>") 'clution-run)
    (define-key map (kbd "C-S-<f5>") 'clution-maybe-restart-repl)
    map))

(define-minor-mode clution-mode
  "minor mode for editing a clution project."
  nil nil nil
  :global t
  :after-hook
  (progn
    (cond
     (clution-mode
      (add-hook 'find-file-hook 'clution--find-file-hook))
     (t
      (clution-close)
      (remove-hook 'find-file-hook 'clution--find-file-hook)))))


(define-derived-mode clution-file-mode lisp-mode
  "clution-file"
  "Major mode for editing a clution project file.")

(define-derived-mode cuo-file-mode lisp-mode
  "cuo-file"
  "Major mode for editing a clution user options file")

(defvar clution-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'clution-close-output)
    (define-key map (kbd "Q") 'clution-close)
    map))

(define-derived-mode clution-output-mode compilation-mode
  "clution-output"
  "Mode for the clution output buffer"
  (setq buffer-read-only t
        truncate-lines -1))

(defvar clutex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'clution-close-clutex)
    (define-key map (kbd "Q") 'clution-close)
    map))

(define-derived-mode clutex-mode special-mode "ClutexMode"
  "A major mode for displaying the directory tree in a clution."
  (setq indent-tabs-mode nil
        buffer-read-only t
        truncate-lines -1))

;;; Functions

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

(defun clution-end-repl ()
  (interactive)
  (cond
   (*clution--current-op*
    (message "clution: busy doing op: '%s'" (getf *clution--current-op* :type)))
   (*clution--repl-active*
    (clution--end-repl))
   (t
    (message "clution: no repl active"))))

(defun clution-maybe-restart-repl ()
  (interactive)

  (cond
   (*clution--current-op*
    (message "clution: busy doing op: '%s'" (getf *clution--current-op* :type)))
   (*clution--repl-active*
    (clution--clear-output)
    (clution--append-output
     "Retarting repl for: '" (clution--clution.name *clution--current-clution*)
     "'\n\n")
    (clution--restart-repl))
   (t
    (clution-repl))))

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
   (*clution--repl-active*
    (clution--end-repl)
    (clution-run))
   (*clution--current-clution*
    (clution--clear-output)
    (if (clution--system.toplevel (clution--clution.selected-system))
        (add-hook 'clution-build-complete-hook 'clution--run-on-build-complete)
      (clution--append-output "clution: warning: no toplevel available for '" (clution--system.name (clution--clution.selected-system)) "'\n"))
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

(defun clution-create-clution (path)
  (interactive
   (list (clution--read-new-file-name "path to new clution: ")))
  (let ((clution (clution--make-clution (list) path)))
    (clution--save-clution clution)))

(defun clution-open (path)
  (interactive
   (list (clution--read-file-name "clution to open: " nil nil t)))

  (when *clution--current-clution*
    (let ((clution-intrusive-ui (not clution-intrusive-ui)))
      (clution-close)))

  (let ((path (expand-file-name path)))
    (setf *clution--current-clution*
          (clution--parse-file path))
    (setf *clution--current-watch*
          (file-notify-add-watch path '(change) 'clution--file-watch-callback))

    (clution--sync-buffers *clution--current-clution*))

  (when clution-intrusive-ui
    (clution-open-output)
    (clution-open-clutex))

  (run-hooks 'clution-open-hook))

(defun clution-open-asd (path)
  (interactive
   (list (clution--read-file-name "asd to open: " nil nil t)))

  (when *clution--current-clution*
    (clution-close))

  (let ((path (expand-file-name path)))
    (setf *clution--current-clution* (clution--make-asd-clution path))
    (when-let ((clution-path (clution--clution.path *clution--current-clution*)))
      (clution--save-clution *clution--current-clution*)
      (when (file-exists-p clution-path)
        (setf *clution--current-watch* (file-notify-add-watch clution-path '(change) 'clution--file-watch-callback))))

    (clution--sync-buffers *clution--current-clution*))

  (when clution-intrusive-ui
    (clution-open-clutex)
    (clution-open-output))

  (run-hooks 'clution-open-hook))

(defun clution-close ()
  (interactive)
  (when *clution--current-clution*
    (when *clution--repl-active*
      (clution-end-repl))

    (clution--sync-buffers nil)

    (when *clution--current-watch*
      (file-notify-rm-watch *clution--current-watch*)
      (setf *clution--current-watch* nil))

    (setf *clution--current-clution* nil)

    (when clution-intrusive-ui
      (clution-close-clutex)
      (clution-close-output))

    (run-hooks 'clution-close-hook)))

(defun clution-output-default-display-fn (buffer _alist)
  "Display BUFFER to the bottom of the root window.
The root window is the root window of the selected frame.
_ALIST is ignored."
  (display-buffer-in-side-window buffer '((side . bottom))))

(defun clution-repl-default-display-fn (buffer _alist)
  "Display BUFFER to the bottom of the root window, on the left slot.
The root window is the root window of the selected frame.
_ALIST is ignored."
  (display-buffer-in-side-window buffer '((side . bottom) (slot . -1))))

(defun clution-open-output ()
  (interactive)
  (unless (window-live-p *clution--output-window*)
    (let ((buffer (clution--output-buffer t)))
      (clution--sync-output *clution--current-clution* buffer)
      (setf *clution--output-window*
            (display-buffer buffer '(clution-output-default-display-fn))))
    (clution--init-output-window *clution--output-window*)))

(defun clution-close-output ()
  (interactive)
  (when (window-live-p *clution--output-window*)
    (delete-window *clution--output-window*))
  (setf *clution--output-window* nil)
  (when-let ((buffer (clution--output-buffer)))
    (clution--kill-buffer-if-no-window buffer)))

(defun clution-clutex-default-display-fn (buffer _alist)
  "Display BUFFER to the left or right of the root window.
The side is decided according to `clution-clutex-position'
The root window is the root window of the selected frame.
_ALIST is ignored."
  (let ((window-pos (if (eq clution-clutex-position 'right) 'right 'left)))
    (display-buffer-in-side-window buffer `((side . ,window-pos)))))

(defun clution-open-clutex ()
  (interactive)
  (unless (window-live-p *clution--clutex-window*)
    (let ((buffer (clution--clutex-buffer t)))
      (clution--sync-clutex *clution--current-clution* buffer)
      (setf *clution--clutex-window*
            (display-buffer buffer '(clution-clutex-default-display-fn))))
    (clution--init-clutex-window *clution--clutex-window*)))

(defun clution-close-clutex ()
  (interactive)
  (when (window-live-p *clution--clutex-window*)
    (delete-window *clution--clutex-window*))
  (setf *clution--clutex-window* nil)
  (when-let ((buffer (clution--clutex-buffer)))
    (clution--kill-buffer-if-no-window buffer)))

(add-to-list 'auto-mode-alist '("\\.clu$" . clution-file-mode))
(add-to-list 'auto-mode-alist '("\\.cuo$" . cuo-file-mode))

(provide 'clution)
