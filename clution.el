;;; -*- lexical-binding: t -*-
;;; clution.el --- Common Lisp IDE

;; Copyright (C) 2018

;; Author: Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;; Maintainer: Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;; Created: 01 Jan 2018
;; Version: 0.1.0
;; Keywords: languages
;; Homepage: https://github.com/Zulu-Inuoe/clution
;; Package-Requires: ((emacs "25"))
;; License: CC0
;;    To the extent possible under law, the author(s) have dedicated all copyright
;;    and related and neighboring rights to this software to the public domain
;;    worldwide. This software is distributed without any warranty.
;;    You should have received a copy of the CC0 Public Domain Dedication along
;;    with this software. If not, see
;;    <http://creativecommons.org/publicdomain/zero/1.0/>.

;;; Commentary:
;; clution.el is an IDE package for Common Lisp development, which provides front-ends
;; to a variety of tools for developing Common Lisp projects.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'pp)
(require 'subr-x)

(defconst *clution--cl-clution-path*
  (expand-file-name
   "cl-clution/cl-clution.lisp"
   (file-name-directory (file-truename load-file-name)))
  "Path to the 'cl-clution' script.")

(defconst *clution--ros-script-launcher-path*
  (expand-file-name
   "script-launcher.ros"
   (file-name-directory (file-truename load-file-name)))
  "Path to the `script-launcher` roswell script.")

(defvar *clution--cl-clution-proc* nil
  "A background lisp process to perform operations.")
(defvar *clution--cl-clution-output* "")
(defvar *clution--cl-clution-eval-delim* (format "%dcl-clution-eval-deliml%d" (random) (random)))
(defvar *clution--cl-clution-cont* nil)
(defvar *clution--cl-clution-result* (cons nil nil))

(defun clution--cl-clution-filter (proc string)
  (setf *clution--cl-clution-output* (concat *clution--cl-clution-output* string))
  (when-let ((match-pos (string-match *clution--cl-clution-eval-delim* *clution--cl-clution-output*)))
    (setf *clution--cl-clution-result*
          (condition-case err
              (let ((res (car (read-from-string *clution--cl-clution-output* 0 match-pos))))
                (if (eq (car res) :success)
                    (cons t (cdr res))
                  (cons nil (cdr res))))
            (error
             (warn "error reading from cl-clution")
             (cons nil (format "error during read: %s" err)))))
    (when *clution--cl-clution-cont*
      (run-at-time 0 nil *clution--cl-clution-cont* (car *clution--cl-clution-result*) (cdr *clution--cl-clution-result*)))
    (setf *clution--cl-clution-output* (subseq *clution--cl-clution-output* (+ match-pos (length *clution--cl-clution-eval-delim*))))
    (setf *clution--cl-clution-cont* nil)))

(defun clution--cl-clution-sentinel (proc event)
  (cl-case (process-status proc)
    ((exit closed failed)
     (error "clution--cl-clution died.")
     (clution--cl-clution-start))))

(defun clution--cl-clution-eval-async (sexpr &optional cont)
  (setf *clution--cl-clution-cont* cont)
  (process-send-string
   *clution--cl-clution-proc*
   (format "%S\n" sexpr)))

(defun clution--cl-clution-eval (sexpr)
  (setf *clution--cl-clution-result* nil)
  (process-send-string
   *clution--cl-clution-proc*
   (format "%S\n" sexpr))
  (while (null *clution--cl-clution-result*)
    (accept-process-output *clution--cl-clution-proc*))
  (if (car *clution--cl-clution-result*)
      (cdr *clution--cl-clution-result*)
    (error "%s" (cdr *clution--cl-clution-result*))))

(defun clution--cl-clution-start ()
  (clution--cl-clution-stop)
  (setf *clution--cl-clution-proc*
        (make-process
         :name "*clution-cl-clution*"
         :command (append (clution--spawn-script-command)
                          (clution--spawn-script-args *clution--cl-clution-path*)
                          (list *clution--cl-clution-eval-delim*))
         :filter 'clution--cl-clution-filter
         :sentinel 'clution--cl-clution-sentinel)))

(defun clution--cl-clution-stop ()
  (when *clution--cl-clution-cont*
    (run-at-time 0 nil *clution--cl-clution-cont* nil nil))
  (setf *clution--cl-clution-output* "")
  (setf *clution--cl-clution-cont* nil)
  (when (process-live-p *clution--cl-clution-proc*)
    (set-process-sentinel *clution--cl-clution-proc* nil)
    (delete-process *clution--cl-clution-proc*))
  (setf *clution--cl-clution-proc* nil))

(defun clution--async-proc (&rest args)
  "Run a process asynchronously via `make-process', calling a continuation
function with the exit code when it completes.
Arguments accepted:
  :name - as `make-process'. defaults to \"*clution-async-proc*\"
  :command - as `make-process'
  :dir - the directory to run the process in. Defaults to `default-directory'
  :filter - a filter to attach to the process, as in `make-process'
  :cont - a function of one argument called with the exit code of the process."
  (let ((name (or (cl-getf args :name) "*clution-async-proc*"))
        (command (cl-getf args :command))
        (dir (or (cl-getf args :dir) default-directory))
        (filter (cl-getf args :filter))
        (cont (cl-getf args :cont)))
    (let ((default-directory dir))
      (make-process
       :name name
       :command command
       :filter filter
       :sentinel
       (lambda (proc event)
         (cl-ecase (process-status proc)
           ((stop exit signal closed failed)
            (when cont
              (funcall cont (process-exit-status proc))))
           ((run open connect listen))))))))

(defun clution--translate-system-plist (system system-plist)
  (cl-labels ((translate-component-plist (system parent component-plist)
                                         (let* ((component
                                                 (list
                                                  :system system
                                                  :parent parent
                                                  :name (cl-getf component-plist :name)
                                                  :type (cl-getf component-plist :type)
                                                  :path (file-truename (cl-getf component-plist :pathname))
                                                  :children nil
                                                  :depends-on (cl-getf component-plist :depends-on))))
                                           (setf (cl-getf component :children)
                                                 (cl-mapcar
                                                  (lambda (c)
                                                    (translate-component-plist system component c))
                                                  (cl-getf component-plist :components)))
                                           component)))
    (translate-component-plist system nil system-plist)))

(defun clution--system-query (system)
  "Perform a system query operation on `system' and returns the result."
  (clution--translate-system-plist system (car (clution--cl-clution-eval `(asd-plists ,(clution--system.path system))))))

(defun clution--watch-systems (clution)
  (dolist (system (clution--clution.systems clution))
    (clution--watch-system system)))

(defun clution--unwatch-systems (clution)
  (dolist (system (clution--clution.systems clution))
    (clution--unwatch-system system)))

(defun clution--watch-system (system)
  (push (cons system (file-notify-add-watch (clution--system.path system) '(change) 'clution--system-file-watch-callback))
        *clution--system-watches*))

(defun clution--unwatch-system (system)
  (when-let ((cell (cl-assoc system *clution--system-watches*)))
    (file-notify-rm-watch (cdr cell))
    (setf *clution--system-watches* (cl-delete cell *clution--system-watches*))))

(defun clution--add-directory (clution parent-dir)
  (let ((clu-path (clution--clution.path clution))
        (parent-id (if (null parent-dir) nil (clution--item.id parent-dir)))
        (name (read-string "Directory name: ")))
    (when parent-dir
      ;;Ensure the directory is unfolded in clutex
      (clution--item.set-folded parent-dir nil))
    (clution--cl-clution-eval
     `(add-clu-dir ',clu-path ',parent-id ',name))))

(defun clution--remove-directory (dir)
  (let* ((clution (clution--item.clution dir))
         (clu-path (clution--clution.path clution))
         (id (clution--item.id dir)))
    (when (y-or-n-p (format "confirm: Remove directory '%s'?" (clution--dir.name dir)))
      (clution--cl-clution-eval
       `(remove-clu-dir ',clu-path ',id)))))

(defun clution--add-system (clution parent-dir &optional path type)
  (let ((clu-path (clution--clution.path clution))
        (parent-id (if (null parent-dir) nil (clution--item.id parent-dir)))
        (path (or path (clution--read-file-name "System path: " nil nil t)))
        (type (or type (clution--read-system-type "System type: "))))
    (when parent-dir
      ;;Ensure the directory is unfolded in clutex
      (clution--item.set-folded parent-dir nil))
    (clution--cl-clution-eval
     `(add-clu-system ',clu-path ',parent-id ',path ',type))))

(defun clution--create-system (clution parent-dir)
  (let ((name (string-remove-suffix ".asd" (read-string "System name: ")))
        (dir (read-directory-name "System directory: "))
        (type (clution--read-system-type "System type: ")))
    (let ((path (expand-file-name (concat name ".asd") dir)))
      (when (file-exists-p path)
        (error "clution: system already exists at '%s'" path))
      (if-let ((template-fn (cdr (assoc type clution-system-template-alist))))
          (progn
            (unless (file-exists-p dir)
              (make-directory dir t))
            (funcall template-fn path))
        (error "clution: no template vailable for '%s'" type))
      (cond
       (clution
        (clution--add-system clution parent-dir path type)
        (when parent-dir
          (clution--item.set-folded parent-dir nil)))
       (t
        (clution-open path type)))
      (select-window (clution--clutex-open-file path)))))

(defun clution--select-system (system)
  "Selects the given `system' in the clution.
See `clution--clution.selected-system'"
  (let* ((clution (clution--system.clution system))
         (cuo (clution--clution.cuo clution)))
    (setf (getf cuo :selected-system) (clution--system.name system))
    (clution--save-cuo cuo (clution--cuo.path cuo)))
  (clution--refresh-clutex))

(defun clution--remove-system (system)
  "Remove `system' from its clution."
  (when (y-or-n-p (format "confirm: Remove system '%s'?" (clution--system.name system)))
    (let* ((clution (clution--system.clution system))
           (clu-path (clution--clution.path clution))
           (parent-id (butlast (clution--item.id system)))
           (system-path (clution--system.path system)))
      (clution--cl-clution-eval
       `(remove-clu-system
         ',clu-path
         ;;TODO temp workaround
         ',parent-id
         ',system-path)))))

(defun clution--add-system-file (component)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (dir (clution--component.path component))
         (id (cdr (clution--component.id component)))
         (file (clution--read-file-name "file to add: " dir nil t)))
    ;;Make sure module dir exists
    (unless (file-exists-p dir)
      (make-directory dir t))

    ;;Copy the file over
    (let ((new-name (expand-file-name (file-name-nondirectory file) dir)))
      (cond
       ((not (file-exists-p new-name))
        (copy-file file new-name nil t))
       ((file-equal-p file new-name)
        ;;Same file. Do nothing
        )
       ((y-or-n-p (format "file '%s' already exists. overwrite?" file))
        (copy-file file new-name t t))
       (t ;;do nothing. leave original file.
        ))

      ;;Ensure the module is unfolded in clutex
      (clution--component.set-folded component nil)

      ;;If it's a lisp file, add it as a :file, otherwise a :static-file
      (let ((ext (file-name-extension file t)))
        (cond
         ((string= ext ".lisp")
          (setf new-name (string-remove-suffix ".lisp" new-name))
          (let ((new-name-rel-to-module
                 (file-relative-name new-name dir)))
            (clution--cl-clution-eval
             `(add-file-component ',system-path ',id ',new-name-rel-to-module))))
         (t
          (let ((new-name-rel-to-module
                 (file-relative-name new-name dir)))
            (clution--cl-clution-eval
             `(add-static-file-component ',system-path ',id ',new-name-rel-to-module)))))))))

(defun clution--create-system-file (component)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (dir (clution--component.path component))
         (id (cdr (clution--component.id component)))
         (file (expand-file-name (clution--read-file-name "new file name: " dir) dir))
         (is-static-file
          (or (and (null (file-name-extension file))
                   (string= (file-name-base file) "qlfile"))
              (and (file-name-extension file)
                   (not (string= (file-name-extension file) "lisp"))))))

    ;;Ensure the module is unfolded in clutex
    (clution--component.set-folded component nil)

    (unless is-static-file
      ;;Ensure it has a .lisp extension
      (setf file (concat (file-name-sans-extension file) ".lisp")))

    (when (or (not (file-exists-p file))
              (y-or-n-p (format "file '%s' already exists. overwrite?" file)))
      ;;Ensure the target directory exists
      (let ((file-dir (file-name-directory file)))
        (unless (file-exists-p file-dir)
          (make-directory file-dir t)))
      (cond
       (is-static-file
        (with-temp-buffer
          (write-region nil nil file nil nil nil t)))
       (t
        (with-temp-buffer
          (insert
           (format "(in-package #:%s)\n" (clution--system.name system)))
          (write-region nil nil file nil nil nil t)))))

    (cond
     (is-static-file
      (let ((file-rel-to-module
             (file-relative-name file dir)))
        (clution--cl-clution-eval
         `(add-static-file-component ',system-path ',id ',file-rel-to-module))))
     (t
      (let ((file-no-extension-rel-to-module
             (file-relative-name
              (file-name-sans-extension file)
              dir)))
        (clution--cl-clution-eval
         `(add-file-component ',system-path ',id ',file-no-extension-rel-to-module)))))))

(defun clution--create-system-module (component)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (dir (clution--component.path component))
         (id (cdr (clution--component.id component)))
         (module (expand-file-name (read-directory-name "new module name: " dir))))

    ;;Create the directory if it does not exist
    (unless (file-exists-p module)
      (make-directory module t))

    ;;Ensure the module is unfolded in clutex
    (clution--component.set-folded component nil)

    (let ((name-rel-to-module
           (directory-file-name
            (file-relative-name module dir))))
      (clution--cl-clution-eval
       `(add-module-component ',system-path ',id ',name-rel-to-module)))))

(defun clution--move-component-up (component)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (id (cdr (clution--component.id component))))
    (clution--cl-clution-eval
     `(move-component-up ',system-path ',id))))

(defun clution--move-component-down (component)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (id (cdr (clution--component.id component))))
    (clution--cl-clution-eval
     `(move-component-down ',system-path ',id))))

(defun clution--rename-component (component)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (parent (clution--component.parent component))
         (parent-dir (file-name-as-directory (clution--component.path parent)))
         (old-name (clution--component.name component))
         (old-path (clution--component.path component))
         (id (cdr (clution--component.id component)))
         (component-type (clution--component.type component))
         (new-name
          (let ((str (read-string (format "'%s' rename to: " old-name))))
            (cl-ecase component-type
              (:file (string-remove-suffix ".lisp" str))
              (:module (directory-file-name (file-name-as-directory str)))
              (:static-file str))))
         (new-path
          (expand-file-name
           (cl-ecase component-type
             (:file (concat new-name ".lisp"))
             (:module (file-name-as-directory new-name))
             (:static-file new-name))
           parent-dir)))
    (when (file-exists-p old-path)
      (cl-ecase component-type
        (:file
         (unless (file-exists-p (file-name-directory new-path))
           (make-directory (file-name-directory new-path) t)))
        (:module
         (unless (file-exists-p (file-name-directory (directory-file-name new-path)))
           (make-directory (file-name-directory (directory-file-name new-path)))))
        (:static-file
         (unless (file-exists-p (file-name-directory new-path))
           (make-directory (file-name-directory new-path) t))))
      (rename-file old-path new-path))

    (clution--cl-clution-eval `(rename-component ',system-path ',id ',new-name))))

(defun clution--remove-component (component &optional delete)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (name (clution--component.name component))
         (path (clution--component.path component))
         (id (cdr (clution--component.id component))))
    (cond
     ((not delete)
      (when (y-or-n-p (format "confirm: Remove component '%s'? (%s)" name path))
        (clution--cl-clution-eval `(remove-component ',system-path ',id))))
     (t
      (when (y-or-n-p (format "confirm: Permanently delete component '%s'? (%s)" name path))
        (if (directory-name-p path)
            (delete-directory path t)
          (delete-file path))
        (clution--cl-clution-eval `(remove-component ',system-path ',id)))))))

(defun clution--add-system-dependency (component)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (id (cdr (clution--component.id component)))
         (dependency (read-string "dependency to add: ")))

    (clution--cl-clution-eval
     `(add-depends-on ',system-path ',id ',dependency))))

(defun clution--remove-system-dependency (component dependency)
  (let* ((system (clution--component.system component))
         (system-path (clution--system.path system))
         (id (cdr (clution--component.id component))))

    (clution--cl-clution-eval
     `(remove-depends-on ',system-path ',id ',dependency))))

(defvar *clution--current-clution* nil
  "The currently open clution.")

(defvar *clution--current-watch* nil
  "file watch as per `file-notify-add-watch' which monitors the current clution's
file for changes.")

(defvar *clution--system-watches* nil
  "alist of systems to their file watches as per `file-notify-add-watch',
which monitor the current clution's asd files for changes.")

(defvar *clution--repl-active* nil
  "non-nil if there is a clution repl currently active.")

(defvar *clution--current-op* nil
  "A plist describing clution's current operation, if any.
example:
  (:type clution-build :build-systems (...))")

;;; Buffer and window manipulation
(defvar *clution--output-window* nil
  "The clution output window.")

(defvar *clution--repl-window* nil
  "The clution repl window.")

(defvar *clution--clutex-window* nil
  "The clution clutex window.")

(defun clution--clutex-open-file (path)
  "If `path' points to a file that is currently open in a visible buffer,
select that window.
Otherwise, open that file in the most recently used window without selecting it
see `get-mru-window'
Returns the window displaying the buffer"
  (let ((buffer (find-file-noselect path)))
    (if-let ((existing-window (get-buffer-window buffer)))
        (select-window existing-window)
      (if-let ((mru (get-mru-window))
               (live (window-live-p mru)))
          (with-selected-window mru
            (switch-to-buffer buffer)
            mru)
        (get-buffer-window (switch-to-buffer buffer))))))

(defun clution--next-line-button ()
  "Moves the cursor to the first button on the next line."
  (interactive)
  (when-let ((button (next-button (line-beginning-position 2))))
    (goto-char (button-start button))))

(defun clution--prev-line-button ()
  "Moves the cursor to the first button on the previous line."
  (interactive)
  (when-let ((button (next-button (line-beginning-position 0) t)))
    (goto-char (button-start button))))

(defun clution--clutex-invoke-line ()
  "Invokes the key sequence on the first button on the current line."
  (interactive)
  (when-let* ((button (or (button-at (point))
                          (next-button (line-beginning-position))))
              (keymap (button-get button 'keymap))
              (fn (lookup-key keymap (this-command-keys-vector))))
    (call-interactively fn)))

(cl-defmacro clution--defining-lkeys (map &body keys-and-body)
  "Shorthand for making simple lambda-based keybindings"
  (let ((map-sym (gensym "MAP")))
    `(let ((,map-sym ,map))
       ,@(mapcar (lambda (p) `(define-key ,map-sym (kbd ,(car p)) (lambda () (interactive) ,@(cdr p)))) keys-and-body))))

(defun clution--insert-clution-button (clution indent)
  (insert-char ?\s indent)
  (let* ((map (make-sparse-keymap))
         (button
          (insert-button
           (clution--clution.name clution)
           'face 'clution-clutex-clution-face
           'help-echo (clution--clution.path clution)
           'keymap map)))
    (clution--defining-lkeys
     map
     ("C-m" (clution--clutex-open-file (clution--clution.path clution)))
     ("<double-down-mouse-1>" (clution--clutex-open-file (clution--clution.path clution))))

    (define-key map (kbd "A") 'clution-add-system)
    (define-key map (kbd "N") 'clution-create-system)
    (define-key map (kbd "C-S-N") 'clution-create-directory)
    (let ((mouse-menu (make-sparse-keymap)))
      (define-key map (kbd "<mouse-3>")
        mouse-menu)

      (define-key-after mouse-menu [build]
        '(menu-item "Build" clution-build))
      (define-key-after mouse-menu [clean]
        '(menu-item "Clean" clution-clean))
      (define-key-after mouse-menu [publish]
        '(menu-item "Publish" clution-publish))

      (define-key-after mouse-menu [separator-add]
        '(menu-item "--"))

      (let ((add-menu (make-sparse-keymap)))
        (define-key-after mouse-menu [add]
          `(menu-item "Add" ,add-menu))
        (define-key-after (lookup-key mouse-menu [add]) [add-system]
          '(menu-item "Existing System..." clution-add-system))
        (define-key-after (lookup-key mouse-menu [add]) [create-system]
          '(menu-item "New System..." clution-create-system))
        (define-key-after (lookup-key mouse-menu [add]) [create-directory]
          '(menu-item "New Directory..." clution-create-directory)))

      (define-key-after mouse-menu [separator-open]
        '(menu-item "--"))

      (define-key-after mouse-menu [open]
        `(menu-item "Open"
                    ,(lambda ()
                       (interactive)
                       (clution--clutex-open-file (clution--clution.path clution)))))))
  (insert "\n")
  (dolist (item (clution--clution.items clution))
    (clution--insert-item-button item (+ indent 2))))

(defun clution--insert-item-button (item indent)
  (cl-ecase (clution--item.type item)
    (:dir
     (clution--insert-dir-button item indent))
    (:system
     (clution--insert-system-button item indent))))

(defun clution--insert-dir-button (dir indent)
  (insert-char ?\s indent)
  (let* ((clution (clution--item.clution dir))
         (fold-map (make-sparse-keymap))
         (folded (clution--item.folded dir))
         (fold-button
          (insert-button
           (if folded "▸ " "▾ ")
           'face 'clution-clutex-dir-face
           'help-echo nil
           'keymap fold-map))
         (map (make-sparse-keymap))
         (button
          (insert-button
           (concat (clution--dir.name dir) "/")
           'face 'clution-clutex-dir-face
           'help-echo nil
           'keymap map)))
    (clution--defining-lkeys
     fold-map
     ("C-m" (clution--toggle-item-fold dir))
     ("TAB" (clution--toggle-item-fold dir))
     ("<mouse-1>" (clution--toggle-item-fold dir))
     ("N" (clution--create-system clution dir))
     ("A" (clution--add-system clution dir))
     ("C-S-N" (clution--add-directory clution dir))
     ("D" (clution--remove-directory dir))
     ("<delete>" (clution--remove-directory dir))
     ("S-<delete>" (clution--remove-directory dir))
     ("<C-up>" (clution--move-item-up dir))
     ("<C-down>" (clution--move-item-down dir))
     ("R" (clution--rename-dir dir)))
    (clution--defining-lkeys
     map
     ("C-m" (clution--toggle-item-fold dir))
     ("TAB" (clution--toggle-item-fold dir))
     ("<mouse-1>" (clution--toggle-item-fold dir))
     ("A" (clution--add-system clution dir))
     ("N" (clution--create-system clution dir))
     ("C-S-N" (clution--add-directory clution dir))
     ("D" (clution--remove-directory dir))
     ("<delete>" (clution--remove-directory dir))
     ("S-<delete>" (clution--remove-directory dir))
     ("<C-up>" (clution--move-item-up dir))
     ("<C-down>" (clution--move-item-down dir))
     ("R" (clution--rename-dir dir)))
    (let ((mouse-menu (make-sparse-keymap)))
      (define-key fold-map (kbd "<mouse-3>")
        mouse-menu)
      (define-key map (kbd "<mouse-3>")
        mouse-menu)

      (let ((add-menu (make-sparse-keymap)))
        (define-key-after mouse-menu [add]
          `(menu-item "Add" ,add-menu))
        (define-key-after (lookup-key mouse-menu [add]) [add-system]
          `(menu-item "Existing System..."
                      ,(lambda ()
                         (interactive)
                         (clution--add-system clution dir))))
        (define-key-after (lookup-key mouse-menu [add]) [create-system]
          `(menu-item "New System..."
                      ,(lambda ()
                         (interactive)
                         (clution--create-system clution dir))))
        (define-key-after (lookup-key mouse-menu [add]) [create-directory]
          `(menu-item "New Directory..."
                      ,(lambda ()
                         (interactive)
                         (clution--add-directory clution dir)))))

      (define-key-after mouse-menu [separator-delete]
        '(menu-item "--"))
      (define-key-after mouse-menu [delete]
        `(menu-item "Delete"
                    ,(lambda ()
                       (interactive)
                       (clution--remove-directory dir)))))
    (insert "\n")
    (unless folded
      (dolist (item (clution--dir.items dir))
        (clution--insert-item-button item (+ indent 2))))))

(defun clution--insert-system-button (system indent)
  (insert-char ?\s indent)
  (let* ((clution (clution--system.clution system))
         (selected (eq system (clution--clution.selected-system)))
         (component (clution--system.query-node system))
         (loaded (clution--system.loaded system))
         (folded (clution--item.folded system))
         (fold-map (make-sparse-keymap))
         (fold-button
          (insert-button
           (cond
            ((not loaded) "✘ ")
            (folded "▸ ")
            (t "▾ "))
           'face 'clution-clutex-system-face
           'help-echo nil
           'keymap fold-map))
         (map (make-sparse-keymap))
         (button
          (insert-button
           (clution--system.name system)
           'face (if selected
                     'clution-clutex-selected-system-face
                   'clution-clutex-system-face)
           'help-echo (clution--system.path system)
           'keymap map)))
    (cond
     (loaded
      (clution--defining-lkeys
       fold-map
       ("C-m" (clution--toggle-item-fold system))
       ("TAB" (clution--toggle-item-fold system))
       ("<mouse-1>" (clution--toggle-item-fold system))
       ("<delete>" (clution--remove-system system))
       ("D" (clution--remove-system system))
       ("S" (clution--select-system system))
       ("A" (clution--add-system-file component))
       ("N" (clution--create-system-file component))
       ("C-S-N" (clution--create-system-module component)))
      (clution--defining-lkeys
       map
       ("TAB" (clution--toggle-item-fold system))
       ("<mouse-1>" (clution--toggle-item-fold system))
       ("<delete>" (clution--remove-system system))
       ("D" (clution--remove-system system))
       ("S" (clution--select-system system))
       ("C-m" (clution--clutex-open-file (clution--system.path system)))
       ("<double-down-mouse-1>" (clution--clutex-open-file (clution--system.path system)))
       ("A" (clution--add-system-file component))
       ("N" (clution--create-system-file component))
       ("C-S-N" (clution--create-system-module component)))

      (let ((mouse-menu (make-sparse-keymap)))
        (define-key map (kbd "<mouse-3>")
          mouse-menu)
        (define-key fold-map (kbd "<mouse-3>")
          mouse-menu)

        (define-key-after mouse-menu [build]
          `(menu-item "Build"
                      ,(lambda ()
                         (interactive)
                         (clution-build (list system)))))
        (define-key-after mouse-menu [clean]
          `(menu-item "Clean"
                      ,(lambda ()
                         (interactive)
                         (clution-clean (list system)))))
        (define-key-after mouse-menu [publish]
          `(menu-item "Publish"
                      ,(lambda ()
                         (interactive)
                         (clution-publish system))))

        (define-key-after mouse-menu [separator-add]
          '(menu-item "--"))

        (let ((add-menu (make-sparse-keymap)))
          (define-key-after mouse-menu [add]
            `(menu-item "Add" ,add-menu))
          (define-key-after (lookup-key mouse-menu [add]) [create-file]
            `(menu-item "New file..."
                        ,(lambda ()
                           (interactive)
                           (clution--create-system-file component))))
          (define-key-after (lookup-key mouse-menu [add]) [add-file]
            `(menu-item "Existing file..."
                        ,(lambda ()
                           (interactive)
                           (clution--add-system-file component))))
          (define-key-after (lookup-key mouse-menu [add]) [create-module]
            `(menu-item "New module..."
                        ,(lambda ()
                           (interactive)
                           (clution--create-system-module component)))))

        (define-key-after mouse-menu [separator-select]
          '(menu-item "--"))

        (define-key-after mouse-menu [select]
          `(menu-item "Set as Selected System"
                      ,(lambda ()
                         (interactive)
                         (clution--select-system system))))

        (define-key-after mouse-menu [repl]
          `(menu-item "REPL"
                      ,(lambda ()
                         (interactive)
                         (clution-repl system))))

        (define-key-after mouse-menu [separator-delete]
          '(menu-item "--"))

        (define-key-after mouse-menu [delete]
          `(menu-item "Remove"
                      ,(lambda ()
                         (interactive)
                         (clution--remove-system system))))

        (define-key-after mouse-menu [separator-open]
          '(menu-item "--"))

        (define-key-after mouse-menu [open]
          `(menu-item "Open"
                      ,(lambda ()
                         (interactive)
                         (clution--clutex-open-file (clution--system.path system)))))))
     (t ;;not loaded
      (define-key fold-map (kbd "C-m")
        (lambda ()
          (interactive)
          (clution--update-system-query system)
          (clution--refresh-clutex)))
      (define-key map (kbd "C-m")
        (lambda ()
          (interactive)
          (clution--update-system-query system)
          (clution--refresh-clutex)))
      (define-key fold-map (kbd "<double-down-mouse-1>")
        (lambda ()
          (interactive)
          (clution--update-system-query system)
          (clution--refresh-clutex)))
      (define-key map (kbd "<double-down-mouse-1>")
        (lambda ()
          (interactive)
          (clution--update-system-query system)
          (clution--refresh-clutex)))

      (let ((mouse-menu (make-sparse-keymap)))
        (define-key map (kbd "<mouse-3>")
          mouse-menu)
        (define-key fold-map (kbd "<mouse-3>")
          mouse-menu)

        (define-key-after mouse-menu [reload]
          `(menu-item "Reload"
                      ,(lambda ()
                         (interactive)
                         (clution--update-system-query system)
                         (clution--refresh-clutex)))))))
    (insert "\n")
    (when (and loaded (not folded))
      (clution--insert-depends-on component (+ indent 2))
      (dolist (component (clution--component.children component))
        (clution--insert-component-button component (+ indent 2))))))

(defun clution--insert-component-button (component indent)
  (cl-ecase  (clution--component.type component)
    (:file
     (clution--insert-file-component-button component indent))
    (:module
     (clution--insert-module-component-button component indent))
    (:static-file
     (clution--insert-file-component-button component indent))))

(defun clution--insert-module-component-button (module indent)
  (insert-char ?\s indent)
  (let* ((fold-map (make-sparse-keymap))
         (fold-button
          (insert-button
           (if (clution--component.folded module) "▸ " "▾ ")
           'face 'clution-clutex-dir-face
           'help-echo nil
           'keymap fold-map))
         (map (make-sparse-keymap))
         (button
          (insert-button
           (concat (clution--component.name module) "/")
           'face 'clution-clutex-dir-face
           'help-echo (clution--component.path module)
           'keymap map)))
    (clution--defining-lkeys
     fold-map
     ("C-m" (clution--toggle-component-fold module))
     ("TAB" (clution--toggle-component-fold module))
     ("<mouse-1>" (clution--toggle-component-fold module))
     ("C-m" (clution--toggle-component-fold module))
     ("D" (clution--remove-component module nil))
     ("<delete>" (clution--remove-component module nil))
     ("S-<delete>" (clution--remove-component module t))
     ("A" (clution--add-system-file module))
     ("N" (clution--create-system-file module))
     ("C-S-N" (clution--create-system-module module))
     ("<C-up>" (clution--move-component-up module))
     ("<C-down>" (clution--move-component-down module))
     ("R" (clution--rename-component module)))

    (clution--defining-lkeys
     map
     ("TAB" (clution--toggle-component-fold module))
     ("<mouse-1>" (clution--toggle-component-fold module))
     ("D" (clution--remove-component module nil))
     ("<delete>" (clution--remove-component module nil))
     ("S-<delete>" (clution--remove-component module t))
     ("A" (clution--add-system-file module))
     ("N" (clution--create-system-file module))
     ("C-S-N" (clution--create-system-module module))
     ("<C-up>" (clution--move-component-up module))
     ("<C-down>" (clution--move-component-down module))
     ("R" (clution--rename-component module)))

    (let ((mouse-menu (make-sparse-keymap)))
      (define-key map (kbd "<mouse-3>")
        mouse-menu)
      (define-key fold-map (kbd "<mouse-3>")
        mouse-menu)

      (let ((add-menu (make-sparse-keymap)))
        (define-key-after mouse-menu [add]
          `(menu-item "Add" ,add-menu))

        (define-key-after (lookup-key mouse-menu [add]) [create-file]
          `(menu-item "New file..."
                      ,(lambda ()
                         (interactive)
                         (clution--create-system-file module))))

        (define-key-after (lookup-key mouse-menu [add]) [add-file]
          `(menu-item "Existing file..."
                      ,(lambda ()
                         (interactive)
                         (clution--add-system-file module))))
        (define-key-after (lookup-key mouse-menu [add]) [create-module]
          `(menu-item "New module..."
                      ,(lambda ()
                         (interactive)
                         (clution--create-system-module module)))))

      (define-key-after mouse-menu [separator-move-up]
        '(menu-item "--"))

      (define-key-after mouse-menu [move-up]
        `(menu-item "Move Up"
                    ,(lambda ()
                       (interactive)
                       (clution--move-component-up module))))

      (define-key-after mouse-menu [move-down]
        `(menu-item "Move Down"
                    ,(lambda ()
                       (interactive)
                       (clution--move-component-down module))))

      (define-key-after mouse-menu [separator-delete]
        '(menu-item "--"))

      (define-key-after mouse-menu [delete]
        `(menu-item "Delete"
                    ,(lambda ()
                       (interactive)
                       (clution--remove-component module t)))))
    (insert "\n")
    (unless (clution--component.folded module)
      (dolist (component (clution--component.children module))
        (clution--insert-component-button component (+ indent 2))))))

(defun clution--insert-file-component-button (file indent)
  (insert-char ?\s indent)
  (let* ((map (make-sparse-keymap))
         (button
          (insert-button
           (concat (clution--component.name file)
                   (file-name-extension (clution--component.path file) t))
           'face 'clution-clutex-file-face
           'help-echo (clution--component.path file)
           'keymap map)))
    (clution--defining-lkeys
     map
     ("C-m" (clution--clutex-open-file (clution--component.path file)))
     ("<double-down-mouse-1>" (clution--clutex-open-file (clution--component.path file)))
     ("D" (clution--remove-component file nil))
     ("<delete>" (clution--remove-component file nil))
     ("S-<delete>" (clution--remove-component file t))
     ("A" (clution--add-system-file (clution--component.parent file)))
     ("N" (clution--create-system-file (clution--component.parent file)))
     ("C-S-N" (clution--create-system-module (clution--component.parent file)))
     ("<C-up>" (clution--move-component-up file))
     ("<C-down>" (clution--move-component-down file))
     ("R" (clution--rename-component file)))

    (let ((mouse-menu (make-sparse-keymap)))
      (define-key map (kbd "<mouse-3>")
        mouse-menu)

      (define-key-after mouse-menu [open]
        `(menu-item "Open"
                    ,(lambda ()
                       (interactive)
                       (clution--clutex-open-file (clution--component.path file)))))
      (define-key-after mouse-menu [separator-move-up]
        '(menu-item "--"))

      (define-key-after mouse-menu [move-up]
        `(menu-item "Move Up"
                    ,(lambda ()
                       (interactive)
                       (clution--move-component-up file))))

      (define-key-after mouse-menu [move-down]
        `(menu-item "Move Down"
                    ,(lambda ()
                       (interactive)
                       (clution--move-component-down file))))

      (define-key-after mouse-menu [separator-delete]
        '(menu-item "--"))

      (define-key-after mouse-menu [delete]
        `(menu-item "Delete"
                    ,(lambda ()
                       (interactive)
                       (clution--remove-component file t))))
      (define-key-after mouse-menu [rename]
        `(menu-item "Rename"
                    ,(lambda ()
                       (interactive)
                       (clution--rename-component file))))))
  (insert "\n"))

(defun clution--insert-depends-on (component indent)
  (insert-char ?\s indent)
  (let* ((depends-on (clution--component.depends-on component))
         (fold-map (make-sparse-keymap))
         (fold-button
          (insert-button
           (if (clution--depends-on.folded component) "▸ " "▾ ")
           'face 'clution-clutex-dependencies-face
           'help-echo nil
           'keymap fold-map))
         (map (make-sparse-keymap))
         (button
          (insert-button
           "Dependencies"
           'face 'clution-clutex-dependencies-face
           'help-echo nil
           'keymap map)))
    (clution--defining-lkeys
     fold-map
     ("C-m" (clution--toggle-depends-on-fold component))
     ("TAB" (clution--toggle-depends-on-fold component))
     ("<mouse-1>" (clution--toggle-depends-on-fold component)))
    (clution--defining-lkeys
     map
     ("C-m" (clution--toggle-depends-on-fold component))
     ("TAB" (clution--toggle-depends-on-fold component))
     ("<mouse-1>" (clution--toggle-depends-on-fold component))
     ("A" (clution--add-system-dependency component)))

    (let ((mouse-menu (make-sparse-keymap)))
      (define-key map (kbd "<mouse-3>")
        mouse-menu)
      (define-key fold-map (kbd "<mouse-3>")
        mouse-menu)

      (define-key-after mouse-menu [add]
        `(menu-item "Add Dependency..."
                    ,(lambda ()
                       (interactive)
                       (clution--add-system-dependency component)))))

    (unless (clution--depends-on.folded component)
      (dolist (dependency depends-on)
        (insert "\n")
        (insert-char ?\s (+ indent 2))
        (let* ((dep-map (make-sparse-keymap))
               (dep-button (insert-button
                            dependency
                            'face 'clution-clutex-dependencies-face
                            'help-echo nil
                            'keymap dep-map)))
          (define-key dep-map (kbd "D")
            (lambda ()
              (interactive)
              (clution--remove-system-dependency component dependency)))
          (define-key dep-map (kbd "<delete>")
            (lambda ()
              (interactive)
              (clution--remove-system-dependency component dependency)))
          (define-key dep-map (kbd "A")
            (lambda ()
              (interactive)
              (clution--add-system-dependency component)))

          (let ((mouse-map (make-sparse-keymap)))
            (define-key dep-map (kbd "<mouse-3>")
              mouse-map)

            (define-key-after mouse-map [remove]
              `(menu-item "Remove"
                          ,(lambda ()
                             (interactive)
                             (clution--remove-system-dependency component dependency)))))))))
  (insert "\n"))

(defun clution--populate-clutex (clution buffer)
  (with-current-buffer buffer
    (clution--insert-clution-button clution 0)))

(defun clution--output-buffer (&optional create)
  (let ((buffer (get-buffer " *clution-output*")))
    (when (and (null buffer) create)
      (setf buffer (generate-new-buffer " *clution-output*"))
      (with-current-buffer buffer
        (clution-output-mode)))
    buffer))

(defun clution--clutex-buffer (&optional create)
  (let ((buffer (get-buffer " *clution-clutex*")))
    (when (and (null buffer) create)
      (setf buffer (generate-new-buffer " *clution-clutex*"))
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

(defun clution--read-system-type (prompt)
  (intern (completing-read "System type: " (mapcar #'car clution-system-template-alist) nil t nil 'clution--system-type-history)))

(defun clution--app-data-dir ()
  "Directory for storing per-user clution data files."
  (file-name-as-directory
   (cond
    ((eq clution-app-data-dir 'auto)
     (cond
      ((eq system-type 'windows-nt)
       (expand-file-name
         "clution"
         (getenv "LOCALAPPDATA")))
      (t
       (expand-file-name
        "clution"
        (or (getenv "XDG_DATA_HOME")
            "~/.local/share/")))))
    (t
     clution-app-data-dir))))

(defun clution--asd-clution-dir ()
  "Directory where clution stores asd-generated clution files."
  (file-name-as-directory
   (expand-file-name
    "asd-clution"
    (clution--app-data-dir))))

(defun clution--asd-clution-path (asd-path)
  "Calculate the path to an autogenerated clution from `asd-path'"
  (expand-file-name
   (concat (file-name-base asd-path) ".clu")
   (file-name-as-directory
      (expand-file-name
       (concat (file-name-base asd-path) "-" (secure-hash 'md5 asd-path))
       (clution--asd-clution-dir)))))

(defun clution--set-file-hidden-flag (path &optional hidden)
  (unless (eq system-type 'windows-nt)
    (error "not supported"))
  (let ((clean-path
         (cond
          ((directory-name-p path) (directory-file-name path))
          (t path)))
        (flag (if hidden "+h" "-h")))
    (unless (file-exists-p clean-path)
      (error "file does not exist: %s" path))
    (call-process "attrib" nil nil nil flag clean-path)))

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
        (setf default-directory (clution--clution.default-dir clution))
        (insert "Output buffer for '" (clution--clution.name clution) "' (" (clution--clution.dir clution) ")\n"))
       (t
        (insert "No clution open.\n"))))))

(defun clution--sync-clutex (clution buffer)
  (let ((restoring-point-column
         (when (eq (window-buffer) buffer)
           (cons (window-point) (current-column)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (cond
         (clution
          (setf default-directory (clution--clution.default-dir clution))
          (clution--populate-clutex clution buffer))
         (t
          (insert "No clution open.\n")))))
    (when restoring-point-column
      (set-window-point (selected-window) (car restoring-point-column))
      (move-to-column (cdr restoring-point-column)))))

(defun clution--refresh-clutex ()
  (when-let ((buffer (clution--clutex-buffer)))
    (clution--sync-clutex *clution--current-clution* buffer)
    (clution--kill-buffer-if-no-window buffer)))

(defun clution--refresh-output ()
  (when-let ((buffer (clution--clutex-buffer)))
    (clution--sync-clutex *clution--current-clution* buffer)
    (clution--kill-buffer-if-no-window buffer)))

(defun clution--refresh-output ()
  (when-let ((buffer (clution--clutex-buffer)))
    (clution--sync-clutex *clution--current-clution* buffer)
    (clution--kill-buffer-if-no-window buffer)))

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
  (setf (cl-getf system :query-node)
        (condition-case err
            (clution--system-query system)
          (error
           (warn "clution: error loading system '%s': %s" (clution--system.name system) err)
           nil))))

(defun clution--insert-cuo (cuo indent)
  (insert
   (pp-to-string
    (list
     :selected-system (clution--cuo.selected-system cuo)
     :item-properties (clution--cuo.item-properties cuo)))))

(defun clution--save-cuo (&optional cuo path)
  (unless cuo
    (setq cuo (clution--clution.cuo *clution--current-clution*)))
  (unless path
    (setq path (clution--clution.cuo-path (clution--cuo.clution cuo))))

  (when path
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (clution--insert-cuo cuo 0))))

(defun clution--make-cuo (plist clution)
  (let ((res
         (list
          :clution clution
          :selected-system (cl-getf plist :selected-system)
          :item-properties (cl-getf plist :item-properties))))
    res))

(defun clution--parse-cuo-file (path clution)
  (clution--make-cuo
   (car
    (read-from-string
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string))))
   clution))

(defun clution--save-clution (&optional clution path)
  ;;no-op atm. clution saves after every transaction
  )

(defun clution--make-clution (data)
  (unless (and (listp data)
               (eq (car data) :clution))
    (error "malformed clution data:\n%S" (pp-to-string data)))
  (let* ((plist (cdr data))
         (res
          (list
           :path (cl-getf plist :path)
           :clu-dir (file-name-as-directory (cl-getf plist :clu-dir))
           :qlfile-libs-dir (file-name-as-directory (cl-getf plist :qlfile-libs-dir))
           :cuo nil
           :items nil)))
    (cl-labels ((make-dir-item (plist parent clution)
                               (let ((id-path)
                                     (res
                                      (list
                                       :type :dir
                                       :parent parent
                                       :clution clution
                                       :name (cl-getf plist :name)
                                       :items ())))
                                 (let ((items nil))
                                   (dolist (item (cl-getf plist :items))
                                     (push (make-item item res clution) items))
                                   (setf (cl-getf res :items) (nreverse items)))
                                 res))
                (make-component (plist parent clution)
                                (let ((res
                                       (list
                                        :type :system
                                        :parent parent
                                        :clution clution
                                        :path (cl-getf plist :path)
                                        :toplevel (cl-getf plist :toplevel)
                                        :system-type (cl-getf plist :type)
                                        :query-node nil)))
                                  (clution--update-system-query res)
                                  res))
                (make-item (item parent clution)
                           (unless (listp item)
                             (error "malformed item: %s" item))
                           (cl-ecase (car item)
                             (:dir
                              (make-dir-item (cdr item) parent clution))
                             (:system
                              (make-component (cdr item) parent clution)))))
      (let ((items nil))
        (dolist (item (cl-getf plist :items))
          (push (make-item item nil res) items))
        (setf (cl-getf res :items) (nreverse items))))

    (if (file-exists-p (clution--clution.cuo-path res))
        (setf (cl-getf res :cuo) (clution--parse-cuo-file (clution--clution.cuo-path res) res))
      (setf (cl-getf res :cuo) (clution--make-cuo nil res)))
    res))

(defun clution--parse-file (path)
  (clution--make-clution
   (clution--cl-clution-eval
    `(clu-plist ',path))))

(defun clution--clution.name (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))
  (downcase (file-name-base (clution--clution.path clution))))

(defun clution--clution.path (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))
  (cl-getf clution :path))

(defun clution--clution.items (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))
  (cl-getf clution :items))

(defun clution--clution.systems (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (let ((result ()))
    (cl-labels ((recurse (item)
                         (cl-ecase (clution--item.type item)
                           (:dir
                            (dolist (child (clution--dir.items item))
                              (recurse child)))
                           (:system
                            (push item result)))))
      (dolist (item (clution--clution.items clution))
        (recurse item))
      (nreverse result))))

(defun clution--clution.cuo (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))
  (cl-getf clution :cuo))

(defun clution--clution.selected-system (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (if-let ((system-name (clution--cuo.selected-system (clution--clution.cuo clution))))
      (cl-find system-name (clution--clution.systems clution) :key 'clution--system.name :test 'string-equal)
    (first (clution--clution.systems clution))))

(defun clution--clution.output-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (or (cl-getf clution :output-dir)
      (file-name-as-directory
       (expand-file-name
        "out"
        (clution--clution.default-dir clution)))))

(defun clution--clution.clu-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))
  (cl-getf clution :clu-dir))

(defun clution--clution.cuo-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (expand-file-name
    "cuo"
    (clution--clution.clu-dir clution))))

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
    (clution--clution.clu-dir clution))))

(defun clution--clution.asdf-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-as-directory
   (expand-file-name
    "asdf"
    (clution--clution.clu-dir clution))))

(defun clution--clution.dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (file-name-directory (clution--clution.path clution)))

(defun clution--clution.default-dir (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (cond
   ((and (clution--clution.systems clution)
         (string-equal
          (file-name-directory (directory-file-name (clution--clution.dir)))
          (clution--asd-clution-dir)))
    ;;If it's an asd clution and it has at least one system
    (clution--system.dir
     (or (clution--clution.selected-system clution)
         (first (clution--clution.systems clution)))))
   (t
    (clution--clution.dir clution))))

(defun clution--clution.qlfile-libs-p (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))
  (let ((path (clution--clution.path clution)))
    (clution--cl-clution-eval
     `(has-clu-qlfiles ',path))))

(defun clution--clution.qlfile-libs-up-to-date (&optional clution)
  (unless clution
    (setf clution *clution--current-clution*))

  (let ((path (clution--clution.path clution)))
    (clution--cl-clution-eval
     `(are-clu-qlfile-libs-up-to-date ',path))))

(defun clution--clution.qlfile-libs-dir (&optional clution)
  "Directory where a clution's qlfile libraries are stored."
  (unless clution
    (setf clution *clution--current-clution*))
  (cl-getf clution :qlfile-libs-dir))

(defun clution--cuo.path (cuo)
  (clution--clution.cuo-path (clution--cuo.clution cuo)))

(defun clution--cuo.dir (cuo)
  (file-name-directory (clution--cuo.path cuo)))

(defun clution--cuo.clution (cuo)
  (cl-getf cuo :clution))

(defun clution--cuo.selected-system (cuo)
  (cl-getf cuo :selected-system))

(defun clution--cuo.item-properties (cuo)
  (cl-getf cuo :item-properties))

(defun clution--cuo.get-property (cuo id property &optional default)
  (if-let ((properties (cl-assoc id (clution--cuo.item-properties cuo) :test #'equal)))
      (if-let ((pcell (cl-assoc property properties :test #'equal)))
          (cdr pcell)
        default)
    default))

(defun clution--cuo.set-property (cuo id property value)
  (let ((properties (cl-assoc id (clution--cuo.item-properties cuo) :test #'equal)))
    (unless properties
      (setf properties (cons id nil))
      (push properties (cl-getf cuo :item-properties)))
    (let ((pcell (cl-assoc property (cdr properties) :test #'equal)))
      (unless pcell
        (setf pcell (cons property nil))
        (setf (cdr properties) (push pcell (cdr properties))))
      (setf (cdr pcell) value))))

(defun clution--cuo.get-fold-state (cuo id)
  (clution--cuo.get-property cuo id :folded t))

(defun clution--cuo.set-fold-state (cuo id folded)
  (clution--cuo.set-property cuo id :folded folded)
  (clution--save-cuo cuo))

(defun clution--item.type (item)
  (cl-getf item :type))

(defun clution--item.clution (item)
  (cl-getf item :clution))

(defun clution--item.id (item)
  (let ((parent-id
         (if-let ((parent (clution--item.parent item)))
             (clution--item.id parent)
           nil)))
    (cl-ecase (clution--item.type item)
      (:dir
       (append parent-id (list (clution--dir.name item))))
      (:system
       (append parent-id (list (clution--system.path item)))))))

(defun clution--item.parent (item)
  (cl-getf item :parent))

(defun clution--item.folded (item)
  (let* ((id (clution--item.id item))
         (clution (clution--item.clution item))
         (cuo (clution--clution.cuo clution)))
    (clution--cuo.get-fold-state cuo id)))

(defun clution--item.set-folded (item folded)
  (let* ((id (clution--item.id item))
         (clution (clution--item.clution item))
         (cuo (clution--clution.cuo clution)))
    (clution--cuo.set-fold-state cuo id folded)))

(defun clution--toggle-item-fold (item)
  (clution--item.set-folded item (not (clution--item.folded item)))
  (clution--refresh-clutex))

(defun clution--dir.name (dir)
  (cl-getf dir :name))

(defun clution--dir.items (dir)
  (cl-getf dir :items))

(defun clution--system.path (clution-system)
  (cl-getf clution-system :path))

(defun clution--system.dir (clution-system)
  (file-name-directory (clution--system.path clution-system)))

(defun clution--system.query-node (clution-system)
  (cl-getf clution-system :query-node))

(defun clution--system.query-node-prop (clution-system prop)
  (cl-getf (clution--system.query-node clution-system) prop))

(defun clution--system.name (clution-system)
  (downcase (file-name-base (clution--system.path clution-system))))

(defun clution--system.loaded (clution-system)
  (and (clution--system.query-node clution-system) t))

(defun clution--system.set-name (clution-system name)
  (let ((path (expand-file-name name (clution--system.dir clution-system))))
    (unless (string= (file-name-extension path t) ".asd")
      (setq path (concat path ".asd")))
    (setf (cl-getf clution-system :path) path)))

(defun clution--system.clution (clution-system)
  (cl-getf clution-system :clution))

(defun clution--system.cuo (clution-system)
  (clution--clution.cuo (clution--system.clution clution-system)))

(defun clution--system.toplevel (clution-system)
  (or (cl-getf clution-system :toplevel)
      (format "%s::main" (clution--system.name clution-system))))

(defun clution--system.startup-dir (clution-system)
  (or (cl-getf clution-system :startup-dir)
      (clution--system.dir clution-system)))

(defun clution--system.type (clution-system)
  (cl-getf clution-system :system-type))

(defun clution--system.args (clution-system)
  (let* ((cuo (clution--system.cuo clution-system))
         (id (clution--item.id clution-system)))
    (clution--cuo.get-property cuo id :args nil)))

(defun clution--system.set-args (args clution-system)
  (let* ((cuo (clution--system.cuo clution-system))
         (id (clution--item.id clution-system)))
    (clution--cuo.set-property cuo id :args args)))

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

(defun clution--system.output-dir (clution-system)
  (file-name-as-directory
   (concat
    (clution--clution.output-dir (clution--system.clution clution-system))
    (clution--system.name clution-system))))

(defun clution--component.clution (component)
  (clution--system.clution (clution--component.system component)))

(defun clution--component.system (component)
  (cl-getf component :system))

(defun clution--component.parent (component)
  (cl-getf component :parent))

(defun clution--component.name (component)
  (cl-getf component :name))

(defun clution--component.path (component)
  (cl-getf component :path))

(defun clution--component.type (component)
  (cl-getf component :type))

(defun clution--component.children (component)
  (cl-getf component :children))

(defun clution--component.depends-on (component)
  (cl-getf component :depends-on))

(defun clution--component.id (component)
  (let ((res ())
        (system-id (clution--item.id (clution--component.system component)))
        (component component))
    (while component
      (push (clution--component.name component) res)
      (setf component (clution--component.parent component)))
    (append system-id res)))

(defun clution--component.folded (component)
  (let* ((id (clution--component.id component))
         (clution (clution--component.clution component))
         (cuo (clution--clution.cuo clution)))
    (clution--cuo.get-fold-state cuo id)))

(defun clution--component.set-folded (component folded)
  (let* ((id (clution--component.id component))
         (clution (clution--component.clution component))
         (cuo (clution--clution.cuo clution)))
    (clution--cuo.set-fold-state cuo id folded)))

(defun clution--toggle-component-fold (component)
  (clution--component.set-folded component (not (clution--component.folded component)))
  (clution--refresh-clutex))

(defun clution--depends-on.folded (component)
  (let* ((id (append (clution--component.id component) '("/depends-on/")))
         (clution (clution--component.clution component))
         (cuo (clution--clution.cuo clution)))
    (clution--cuo.get-fold-state cuo id)))

(defun clution--depends-on.set-folded (component folded)
  (let* ((id (append (clution--component.id component) '("/depends-on/")))
         (clution (clution--component.clution component))
         (cuo (clution--clution.cuo clution)))
    (clution--cuo.set-fold-state cuo id folded)))

(defun clution--toggle-depends-on-fold (module)
  (clution--depends-on.set-folded module (not (clution--depends-on.folded module)))
  (clution--refresh-clutex))

;;;; Frontend/Backend Lisp access

(defun clution--spawn-script-command ()
  "Command to spawn a lisp which will load a script file, then exit."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl (clution--sbcl-command))
       (ccl (clution--ccl-command))))
    (roswell
     (append
      (clution--ros-command)
      '("+Q")
      (cl-ecase clution-backend
        (sbcl
         '("--lisp" "sbcl-bin"))
        (ccl
         '("--lisp" "ccl-bin")))
      `("--" ,*clution--ros-script-launcher-path*)))))

(defun clution--spawn-script-args (path)
  "Arguments to spawn a lisp which will load a script file, then exit."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl
        `("--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
          "--noprint" "--disable-debugger" "--load" ,path "--eval" "(sb-ext:exit :code 0)"
          "--end-toplevel-options"))
       (ccl
        `("--batch" "--quiet" "--load" ,path "--eval" "(ccl:quit 0)" "--"))))
    (roswell
     (list path))))

(defun clution--spawn-repl-command ()
  "Command to spawn a lisp in a REPL."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl (clution--sbcl-command))
       (ccl (clution--ccl-command))))
    (roswell (clution--ros-command))))

(defun clution--spawn-repl-args ()
  "Arguments to spawn a lisp in a REPL."
  (cl-ecase clution-frontend
    (raw
     (cl-ecase clution-backend
       (sbcl '("--noinform"))
       (ccl '())))
    (roswell
     (cl-ecase clution-backend
       (sbcl '("run" "--lisp" "sbcl-bin"))
       (ccl '("run" "--lisp" "ccl-bin"))))))

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

(defun clution--sbcl-command ()
  (let ((sbcl-path
         (cond
          ((eq clution-sbcl-path 'auto)
           (let ((search-path exec-path))
             (when-let ((sbcl-home (getenv "SBCL_HOME")))
               (setf search-path (append search-path (list (file-name-as-directory sbcl-home)))))
             (locate-file "sbcl" search-path exec-suffixes 1)))
          (t
           (and (stringp clution-sbcl-path)
                (file-exists-p clution-sbcl-path)
                clution-sbcl-path)))))
    (unless sbcl-path
      (error "sbcl not installed"))

    (list sbcl-path)))

(defun clution--ccl-command ()
  (let ((ccl-path
         (cond
          ((eq clution-ccl-path 'auto)
           (let ((search-path exec-path))
             (when-let ((ccl-home (getenv "CCL_DEFAULT_DIRECTORY")))
               (setf search-path (append search-path (list (file-name-as-directory ccl-home)))))
             (or
              (locate-file "ccl" search-path exec-suffixes 1)
              (when (eq system-type 'windows-nt)
                (or (locate-file "wx86cl64" search-path exec-suffixes 1)
                    (locate-file "wx86cl" search-path exec-suffixes 1)))
              (when (eq system-type 'darwin)
                (or (locate-file "dx86cl64" search-path exec-suffixes 1)
                    (locate-file "dx86cl" search-path exec-suffixes 1)))
              (locate-file "lx86cl64" search-path exec-suffixes 1)
              (locate-file "lx86cl" search-path exec-suffixes 1))))
          (t
           (and (stringp clution-ccl-path)
                (file-exists-p clution-ccl-path)
                clution-ccl-path)))))
    (unless ccl-path
      (error "ccl not installed"))

    (list ccl-path)))

(defun clution--ros-command ()
  (let ((ros-path
         (cond
          ((eq clution-ros-path 'auto)
           (locate-file "ros" exec-path exec-suffixes 1))
          (t
           (and (stringp clution-ros-path)
                (file-exists-p clution-ros-path)
                clution-ros-path)))))
    (unless ros-path
      (error "ros not installed"))

    (list ros-path)))

(defun clution--install-system-searcher-form (clution)
  (let ((names-paths-alist
         (mapcar
          (lambda (system)
            (cons
             (clution--system.name system)
             (clution--system.path system)))
          (clution--clution.systems clution))))
    `(cl:flet ((clution-system-searcher (system-name)
                                        (cl:loop :for (name . path) :in ',names-paths-alist
                                                 :if (cl:string-equal system-name name)
                                                 :return (cl:parse-namestring path))))
              (cl:push (cl:function clution-system-searcher)
                       asdf:*system-definition-search-functions*))))

(defun clution--install-qlfile-libs-searcher-form (clution)
  (let ((dir-search-str (concat
                         (clution--clution.qlfile-libs-dir clution)
                         "**/*.asd")))
    `(cl:flet ((clution-qlfile-libs-searcher (system-name)
                                             (cl:loop
                                              :for path :in (cl:directory ,dir-search-str)
                                              :if (cl:string-equal system-name (cl:pathname-name path))
                                              :return path)))
              (cl:push (cl:function clution-qlfile-libs-searcher)
                       asdf:*system-definition-search-functions*))))

(defun clution--install-output-translations-form (clution)
  (let ((system-output-translations
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
    `(asdf:initialize-output-translations
      (cl:append
       (cl:list*
        :output-translations
        :inherit-configuration
        (cl:mapcar
         (cl:lambda (mapping)
                    (cl:mapcar (cl:function cl:parse-namestring) mapping))
         ',system-output-translations))
       asdf:*output-translations-parameter*))))

(defun clution--build-form (system force)
  "Form to perform a build operation on `system'
Initializes ASDF and builds the given system."
  (let ((clution (clution--system.clution system)))
    `(cl:progn
      (cl:handler-case
       (cl:progn
        ,(clution--install-qlfile-libs-searcher-form clution)
        ,(clution--install-system-searcher-form clution)
        ,(clution--install-output-translations-form clution)
        (cl:let* ((cl:*standard-input* (cl:make-string-input-stream "")))
                 (asdf:compile-system ,(clution--system.name system) :force ,force :verbose nil))
        ,(clution--exit-form 0))
       (cl:error (e)
                 (cl:format cl:*error-output* "error during build:~%~T~A" e)
                 ,(clution--exit-form 1))))))

(defun clution--run-form (system)
  "Form to initialize a lisp to run the given `system'.
Initializes ASDF and loads the selected system, then calls its toplevel."
  (let ((clution (clution--system.clution system))
        (system-name (clution--system.name system))
        (toplevel (clution--system.toplevel system))
        (args (list* nil (clution--system.args system))))
    `(cl:progn
      (cl:handler-case
       (cl:progn
        (cl:let ((cl:*standard-input* (cl:make-broadcast-stream))
                 (cl:*error-output* (cl:make-broadcast-stream))
                 (cl:*standard-output* (cl:make-broadcast-stream))
                 (cl:*trace-output* (cl:make-broadcast-stream))
                 (cl:*debug-io* (cl:make-broadcast-stream))
                 (cl:*query-io* (cl:make-broadcast-stream)))
                ,(clution--install-qlfile-libs-searcher-form clution)
                ,(clution--install-system-searcher-form clution)
                ,(clution--install-output-translations-form clution)
                (asdf:load-system ,system-name :verbose nil)))
       (cl:error (e)
                 (cl:format *error-output* "Uncaught error while building:~%~T~A" e)
                 ,(clution--exit-form 1)))

      (cl:handler-case
       (cl:let ((ret-code (cl:apply (cl:read-from-string ,toplevel) ',args)))
               (cl:if (cl:integerp ret-code)
                      ,(clution--exit-form 'ret-code)
                      ,(clution--exit-form 0)))
       (cl:error (e)
                 (cl:format *error-output* "Uncaught error while running:~%~T~A" e)
                 ,(clution--exit-form 1))))))

(defun clution--repl-form (system)
  "Form to initialize a repl to the given clution.
Initializes ASDF and loads the selected system."
  (let* ((clution (clution--system.clution system))
         (system-name (clution--system.name system)))
    `(cl:progn
      ,(clution--install-qlfile-libs-searcher-form clution)
      ,(clution--install-system-searcher-form clution)
      ,(clution--install-output-translations-form clution)
      (asdf:load-system ,system-name :verbose nil))))

(defun clution--arglist-to-string (arglist)
  "Transforms a list of arguments into a string suitable for windows cmd"
  (with-temp-buffer
    (let ((first t))
      (dolist (s arglist)
        (if first
            (setq first nil)
          (insert " "))
        (insert "^\""
                (replace-regexp-in-string
                 "\n" "^\n\n"
                 (replace-regexp-in-string
                  "\\((\\|)\\|%\\|!\\|\\^\\|\"\\|<\\|>\\|&\\||\\)"
                  "^\\1"
                  (replace-regexp-in-string "\"" "\\\\\"" s)))
                "^\"")))
    (buffer-string)))

(defun clution--spawn-script (system sentinel)
  (cl-ecase clution-run-style
    (comint
     (let ((clution-run-buffer (get-buffer-create " *clution-run-buffer*")))
       (with-current-buffer clution-run-buffer
         (setq default-directory (clution--system.startup-dir system))
         (setq buffer-read-only nil)
         (erase-buffer)
         (comint-mode))
       (make-process
        :name "clution-spawn-script"
        :buffer clution-run-buffer
        :command (append (clution--spawn-script-command) (clution--spawn-script-args (clution--system.script-path system)))
        :connection-type nil
        :noquery nil
        :sentinel
        (lambda (proc event)
          (funcall sentinel proc event)
          (when (eq (process-status proc) 'exit)
            (with-current-buffer clution-run-buffer
              (let ((status (process-exit-status proc)))
                (insert "\n\nFinished running. Exited with code "
                        (number-to-string status)
                        "(0x" (format "%x" status) ")\n\n"))
              (setq buffer-read-only t)))))
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
                   ;;This is the 'title', when it's in double-quotes
                   (clution--arglist-to-string
                    (list (clution--system.name system)))
                   " "
                   (clution--arglist-to-string
                    (clution--spawn-script-command))
                   " "
                   (clution--arglist-to-string
                    (clution--spawn-script-args (clution--system.script-path system)))))))
            (set-process-sentinel proc sentinel))))))))

(defun clution--clear-output ()
  "As `erase-buffer', performed in the clution output buffer."
  (when-let ((buffer (clution--output-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun clution--append-output (&rest args)
  "As `insert', performed in the clution output buffer."
  (when-let ((buffer (clution--output-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (apply 'insert args)))
    (when-let ((output-window (get-buffer-window buffer)))
      (with-selected-window output-window
        (set-window-point output-window (point-max))))))

(defun clution--do-qlfile-sync (clution &optional cont)
  "Synchronizes the qlfiles of the clution's systems."
  (let ((name (clution--clution.name clution))
        (path (clution--clution.path clution)))
    (cond
     ((clution--clution.qlfile-libs-p clution)
      (clution--append-output "Installing qlfile packages for '" (clution--clution.name clution) "'\n")
      (clution--cl-clution-eval-async
       `(sync-clu-qlfiles ',path)
       (lambda (eval-success result)
         (cond
          (eval-success
           (clution--append-output "Finished installing qlfile packages\n"))
          (t
           (clution--append-output "Error installing qlfile packages:\n\t" (format "%s" result))))
         (when cont
           (funcall cont)))))
     (t
      (when cont
        (funcall cont))))))

(defun clution--do-build (systems &optional cont)
  (clution--append-output
   "Build starting: '" (clution--clution.name (clution--system.clution (first systems)))
   "'\n\n")

  (clution--append-output "Building systems:\n")
  (dolist (system systems)
    (clution--append-output "  " (clution--system.name system) "\n"))
  (clution--append-output "\n")

  (setf *clution--current-op*
        (list
         :type 'clution-build
         :build-systems systems))

  (let ((success nil)
        (clution (clution--system.clution (first systems)))
        continue-build-fn)
    (unwind-protect
        (progn
          (setf continue-build-fn
                (lambda (system)
                  (clution--append-output
                   (clution--system.name system) ": build starting\n\n")
                  (let* ((command (append (clution--spawn-repl-command) (clution--spawn-repl-args)))
                         (proc
                          (clution--async-proc
                           :command command
                           :dir (clution--system.startup-dir system)
                           :filter
                           (lambda (proc string)
                             (clution--append-output string))
                           :cont
                           (lambda (code)
                             (let ((system (pop systems)))
                               (clution--append-output "\n" (clution--system.name system) ": ")
                               (if (zerop code)
                                   (clution--append-output "build completed successfully\n\n")
                                 (clution--append-output
                                  (format "error during build: process exited with code '%d'\n\n" code))))
                             (cond
                              ((null systems)
                               (clution--build-complete clution)
                               (when cont (funcall cont)))
                              (t
                               (funcall continue-build-fn (car systems))))))))
                    (process-send-string proc (format "%S\n" (clution--build-form system t)))
                    proc)))
          (funcall continue-build-fn (car systems))
          (setf success t))
      (unless success
        (setf *clution--current-op* nil)))))

(defun clution--do-script-publish (system &optional cont)
  (let* ((clution (clution--system.clution system))
         (system-name (clution--system.name system))
         (toplevel (clution--system.toplevel system))
         (out-dir (clution--system.output-dir system))
         (out-script-path (expand-file-name (concat system-name ".lisp") out-dir))
         (out-qlfile-libs-dir (file-name-as-directory (expand-file-name "qlfile-libs" out-dir)))
         (out-systems-dir  (file-name-as-directory (expand-file-name "systems" out-dir)))
         (system-names-paths-alist
          (mapcar
           (lambda (system)
             (cons
              (clution--system.name system)
              (concat
               (file-name-as-directory "systems")
               (file-name-as-directory (clution--system.name system))
               (file-name-nondirectory (clution--system.path system)))))
           (clution--clution.systems clution))))
    (clution--append-output
     "Creating script bundle for '" system-name "' at\n"
     "\t" out-dir "\n\n")

    ;;Delete any existing publish
    (when (file-exists-p out-dir)
      (delete-directory out-dir t))
    (make-directory out-dir t)

    ;;bundle qlfile dependencies
    (when (clution--clution.qlfile-libs-p clution)
      (clution--append-output
       "Bundling qlfile packages...\n")
      (make-directory out-qlfile-libs-dir t)
      (dolist (dir (directory-files (clution--clution.qlfile-libs-dir clution) t "[^.|..]"))
        (clution--append-output
         "\n\tBundling package: '" (file-name-nondirectory dir) "'")
        (copy-directory (file-name-as-directory dir) out-qlfile-libs-dir t t nil))

      (clution--append-output
       "\n\nFinished bundling qlfile packages\n\n"))
    ;;bundle clution systems
    (clution--append-output
     "Bundling clution systems...\n")
    (make-directory out-systems-dir t)
    (dolist (system (clution--clution.systems clution))
      (let* ((system-name (clution--system.name system))
             (system-path (clution--system.path system))
             (system-dir (clution--system.dir system))
             (system-out-dir (file-name-as-directory
                              (expand-file-name system-name out-systems-dir))))
        (clution--append-output
         "\n\tBundling '" system-name "'\n")
        (cl-labels ((recurse (component)
                             (let* ((path (clution--component.path component))
                                    (rel-path (file-relative-name path system-dir))
                                    (new-path (expand-file-name rel-path system-out-dir)))
                               (cond
                                ((directory-name-p path)
                                 (unless (file-exists-p new-path)
                                   (make-directory new-path t)))
                                (t
                                 (unless (file-exists-p (file-name-directory new-path))
                                   (make-directory (file-name-directory new-path) t))
                                 (clution--append-output
                                  "\t\tCopying '" path "'\n"
                                  "\t\t\tto '" new-path "'\n")
                                 (copy-file path new-path nil t nil nil)))
                               (cl-mapc #'recurse (clution--component.children component)))))
          (recurse (clution--system.query-node system))
          ;;Copy the asd itself
          (copy-file system-path
                     (expand-file-name (file-name-nondirectory system-path) system-out-dir)
                     nil t nil nil))))
    (clution--append-output
     "\n\nFinished bundling clution systems\n\n")

    (clution--append-output
     "Generating script at '" out-script-path "'\n")
    ;;Create the runner script
    (with-temp-buffer
      (insert
       (pp-to-string
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (handler-bind ((error (lambda (e)
                                   (format *error-output* "Error requiring ASDF:~%~T~A" e))))
             (require 'asdf))
           (handler-bind ((error (lambda (e)
                                   (format *error-output* "Error requiring UIOP:~%~T~A" e))))
             (require 'uiop)))))
      (insert
       (pp-to-string
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (handler-case
               (progn
                 (let ((qlfile-libs-paths
                        (directory (uiop:merge-pathnames* "qlfile-libs/**/*.asd" *load-truename*))))
                   (flet ((clution-qlfile-libs-searcher (system-name)
                                                        (loop
                                                         :for path :in qlfile-libs-paths
                                                         :if (string-equal system-name (pathname-name path))
                                                         :return path)))
                     (push (function clution-qlfile-libs-searcher)
                           asdf:*system-definition-search-functions*)))
                 (let ((clution-systems-alist
                        (mapcar (lambda (p)
                                  (cons (car p) (uiop:merge-pathnames* (cdr p) *load-truename*)))
                                ',system-names-paths-alist)))
                   (flet ((clution-system-searcher (system-name)
                                                   (loop :for (name . path) :in clution-systems-alist
                                                         :if (string-equal system-name name)
                                                         :return (parse-namestring path))))
                     (push (function clution-system-searcher)
                           asdf:*system-definition-search-functions*)))
                 ;;Load our system
                 (let ((*standard-output* (make-broadcast-stream))
                       (*trace-output*    (make-broadcast-stream))
                       (*error-output*    (make-broadcast-stream)))
                   (asdf:load-system ,system-name :verbose nil)))
             (error (e)
                    (format *error-output* "Error loading systems:~%~T~A" e)
                    ,(clution--exit-form -1))))))
      (insert
       (pp-to-string
        `(handler-case
             (let ((ret-code (apply (function ,(intern toplevel)) ,(clution--args-list-form))))
               (if (integerp ret-code)
                   ,(clution--exit-form 'ret-code)
                 ,(clution--exit-form 0)))
           (error (e)
                  (format *error-output* "Uncaught error:~%~T~A" e)
                  ,(clution--exit-form -1)))))
      (write-region nil nil out-script-path))

    (clution--append-output
     "Finished generating script\n\n")

    (clution--append-output
     "Finished bundling '" system-name "'\n\n"))

  (setf *clution--current-op* nil)
  (when cont
    (funcall cont)))

(defun clution--do-exe-publish (system &optional cont)
  (let* ((clution (clution--system.clution system))
         (system-name (clution--system.name system))
         (toplevel (clution--system.toplevel system))
         (out-dir (clution--system.output-dir system))
         (out-exe-path (expand-file-name (if (eq system-type 'windows-nt)
                                             (concat system-name ".exe")
                                           system-name)
                                         out-dir)))
    (clution--append-output
     "Creating executable for '" system-name "' at\n"
     "\t" out-dir "\n\n")

    ;;Delete any existing publish
    (when (file-exists-p out-dir)
      (delete-directory out-dir t))
    (make-directory out-dir t)

    (let ((proc
           (clution--async-proc
            :name "*clution-exe-publish-proc*"
            :command (append (clution--spawn-repl-command) (clution--spawn-repl-args))
            :dir (clution--clution.dir clution)
            :filter (lambda (proc string)
                      (clution--append-output string))
            :cont
            (lambda (code)
              (setf *clution--current-op* nil)
              (if (zerop code)
                  (clution--append-output "\nFinished building '" system-name "'\n\n")
                (clution--append-output
                 (format "\nError building '%s', exited with code %d (0x%x)\n\n" system-name code code)))
              (when cont
                (funcall cont))))))

      (process-send-string
       proc
       (format
        "%S\n"
        `(handler-case
             (require 'asdf)
           (error (e)
                  (format *error-output* "Error requiring ASDF:~%~T~A" e)
                  ,(clution--exit-form 1)))))
      (process-send-string
       proc
       (format
        "%S\n"
        `(handler-case
             (progn
               ,(clution--install-qlfile-libs-searcher-form clution)
               ,(clution--install-system-searcher-form clution)
               ,(clution--install-output-translations-form clution)

               ;;Load our system
               (asdf:load-system ,system-name :verbose t))
           (error (e)
                  (format *error-output* "clution: error: loading systems:~%~T~A" e)
                  ,(clution--exit-form 1)))))
      (process-send-string
       proc
       (format
        "%S\n"
        `(progn
           (handler-case
               (progn
                 (unless (fboundp ',(intern toplevel))
                   (format *error-output* "clution: error: toplevel is not fboundp~%")
                   ,(clution--exit-form 1))
                 (defun clution-entry-point ()
                   (handler-case
                       (let ((ret-code (apply (function ,(intern toplevel)) ,(clution--args-list-form))))
                         (if (integerp ret-code)
                             ,(clution--exit-form 'ret-code)
                           ,(clution--exit-form 0)))
                     (error (e)
                            (format *error-output* "Uncaught error:~%~T~A" e)
                            ,(clution--exit-form 1))))
                 (setf uiop:*image-entry-point* #'clution-entry-point))
             (error (e)
                    (format *error-output* "clution: error: establishing toplevel~%~T~A" e)
                    ,(clution--exit-form 1)))
           (uiop:dump-image
            ,out-exe-path
            :executable t)))))))

(defun clution--do-publish (system &optional cont)
  (clution--append-output
   "Publish starting: '" (clution--system.name system) "'\n\n")

  (setf *clution--current-op*
        (list
         :type 'clution-publish
         :publish-system system))

  (let ((success nil))
    (unwind-protect
        (if-let ((handler (cdr (assoc (clution--system.type system) clution-publish-alist))))
            (funcall handler system cont)
          (error "clution: no publish handler defined for system type '%s'" (clution--system.type system)))
      (unless success
        (setf *clution--current-op* nil)))))

(defun clution--repl-sly-compile (systems)
  "Performs a build operation in a sly repl."
  (let* ((clution (clution--system.clution (car systems)))
         (system-names (mapcar 'clution--system.name systems)))
    (sly-eval-async
        `(cl:progn
          (slynk::with-buffer-syntax
           ()
           (slynk::collect-notes
            (cl:lambda (cl:&aux (success cl:t))
                       (cl:dolist (system-name ',system-names success)
                                  (cl:handler-case
                                   (slynk::with-compilation-hooks ()
                                                                  (asdf:compile-system system-name :force cl:nil))
                                   (asdf:compile-error ()
                                                       (cl:setf success cl:nil))
                                   (asdf/lisp-build:compile-file-error ()
                                                                       (cl:setf success cl:nil))))))))
      (lambda (result)
        (let ((default-directory (clution--clution.default-dir clution)))
          (sly-compilation-finished result nil)
          (clution--append-output (with-current-buffer (sly-buffer-name :compilation) (buffer-string)) "\n\n")
          (clution--build-complete clution)))
      "COMMON-LISP-USER")))

(defun clution--repl-slime-compile (systems)
  "Performs a build operation in a slime repl."
  (let* ((clution (clution--system.clution (car systems)))
         (system-names (mapcar 'clution--system.name systems)))
    (slime-eval-async
     `(cl:progn
       (swank::collect-notes
        (cl:lambda (cl:&aux (success cl:t))
                   (cl:dolist (system-name ',system-names success)
                              (cl:handler-case
                               (swank::with-compilation-hooks ()
                                                              (asdf:compile-system system-name :force t))
                               (asdf:compile-error ()
                                                   (cl:setf success cl:nil))
                               (asdf/lisp-build:compile-file-error ()
                                                                   (cl:setf success cl:nil)))))))
     (lambda (result)
       (let ((default-directory (clution--clution.default-dir clution)))
         (slime-compilation-finished result)
         (clution--append-output (with-current-buffer (slime-buffer-name :compilation) (buffer-string)) "\n\n")
         (clution--build-complete clution)))
     "COMMON-LISP-USER")))

(defun clution--kickoff-build-in-repl (systems)
  "Performs a build operation in the currently running repl."
  (clution--append-output
   "Build starting: '" (clution--clution.name (clution--system.clution (first systems)))
   "'\n\n")

  (clution--append-output "Building systems:\n")
  (dolist (system systems)
    (clution--append-output "  " (clution--system.name system) "\n"))
  (clution--append-output "\n")

  (setf *clution--current-op*
        (list
         :type 'clution-build-in-repl
         :build-systems systems))

  (let ((success nil))
    (unwind-protect
        (let ((system-names (mapcar 'clution--system.name systems)))
          (cl-ecase clution-repl-style
            (sly
             (clution--repl-sly-compile systems))
            (slime
             (clution--repl-slime-compile systems)))
          (setf success t))
      (unless success
        (setf *clution--current-op* nil)))))

(defun clution--build-complete (clution)
  "Called when a buile operation completes."
  (clution--append-output
   "Build Complete: '" (clution--clution.name clution) "'\n")

  (setf *clution--current-op* nil)
  (run-hooks 'clution-build-complete-hook))

(defun clution--do-run (system &optional cont)
  (let* ((clution (clution--system.clution system))
         (script-path (clution--system.script-path system))
         (script-dir (file-name-directory script-path)))
    (clution--append-output
     "Running: '" (clution--clution.name clution)
     "' system: '" (clution--system.name system)
     "' toplevel: '" (clution--system.toplevel system)
     "'\n\n")

    (setf *clution--current-op*
          (list
           :type 'clution-run
           :toplevel (clution--system.toplevel system)))

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
            (clution--spawn-script
             system
             (lambda (proc event)
               (cl-case (process-status proc)
                 (exit
                  (let ((status (process-exit-status proc)))
                    (clution--append-output
                     (format "Finished running. Exited with code %d(0x%x)\n\n" status status)))
                  (clution--run-complete)
                  (when cont (funcall cont))))))
            (setf success t))
        (unless success
          (setq *clution--current-op* nil))))))

(defun clution--run-complete ()
  "Called when a run operation completes."
  (setf *clution--current-op* nil)
  (run-hooks 'clution-run-complete-hook))

(defun clution--do-clean (systems &optional cont)
  "Perform a clean operation on each system in `systems'"
  (setf *clution--current-op*
        (list
         :type 'clution-clean
         :build-systems systems))
  (dolist (system systems)
    (let ((system-cache-dir (clution--system.cache-dir system)))
      (when (file-exists-p system-cache-dir)
        (clution--append-output
         "Removing '" (clution--system.name system)
         "' asdf cache: '" system-cache-dir
         "'\n\n")

        (delete-directory system-cache-dir t))))
  (setf *clution--current-op* nil)
  (when cont
    (funcall cont)))

(defun clution--display-in-repl-window (buffer)
  "Display `buffer' in the clution repl window, and select it."
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
  "Advice function for sly to pop up the sly-mrepl in the repl window."
  ;;Remove advice. We only need to do this once.
  (advice-remove 'sly-mrepl-on-connection 'clution--sly-mrepl-on-connection-advice)

  ;;Let sly-mrepl et al initialize
  (save-window-excursion
    (funcall orig-fun))

  ;;Display sly-mrepl in the repl window
  (clution--display-in-repl-window (sly-mrepl)))

(defun clution--start-sly-repl (system)
  "Start a sly style repl."
  (when (and clution-intrusive-ui
             (featurep 'sly-mrepl))
    (advice-add 'sly-mrepl-on-connection :around 'clution--sly-mrepl-on-connection-advice))

  ;;Set up hooks for successful connect and failed start
  (let (connected-hook start-failed-hook)
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
            (let (net-close-hook)
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

  (let* ((command (append (clution--spawn-repl-command) (clution--spawn-repl-args)))
         (sly-inferior-buffer
          (sly-start
           :program (first command)
           :program-args (rest command)
           :directory (clution--system.startup-dir system)
           :init
           (lambda (port-filename coding-system)
             (format "(progn %s %S)\n\n"
                     (funcall sly-init-function port-filename coding-system)
                     (clution--repl-form system)))))
         (sly-inferior-process (get-buffer-process sly-inferior-buffer))
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
                            (funcall prev-sentinel proc event)))

    (when (and clution-intrusive-ui
               clution-show-inferior-start)
      (clution--display-in-repl-window sly-inferior-buffer))))

(defun clution--slime-repl-connected-hook-function-advice (orig-fun)
  "Advice function for slime to pop up the slime repl in the repl window."
  ;;Remove advice. We only need to run once.
  (advice-remove 'slime-repl-connected-hook-function 'clution--slime-repl-connected-hook-function-advice)

  ;;Slime will switch out inferior-lisp for slime-repl
  ;;undedicate the repl window temporarily so it doesn't fail.
  (when (window-live-p *clution--repl-window*)
    (set-window-dedicated-p *clution--repl-window* nil))

  ;;Allow the repl to spin up etc
  (save-window-excursion
    (funcall orig-fun))

  ;;Show the slime repl in the clution repl window.
  (clution--display-in-repl-window (slime-repl-buffer)))

(defun clution--start-slime-repl (system)
  "Start a slime style repl."
  ;;Set up advice for switching to the slime repl after connection
  (when (and clution-intrusive-ui
             (featurep 'slime-repl))
    (advice-add 'slime-repl-connected-hook-function :around 'clution--slime-repl-connected-hook-function-advice))

  ;;Set up hooks for successful connect and failed start
  (let (connected-hook start-failed-hook)
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
            (let (net-close-hook)
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

  (let* ((command (append (clution--spawn-repl-command) (clution--spawn-repl-args)))
         (slime-inferior-buffer
          (cl-flet ((do-start ()
                              (slime-start
                               :program (first command)
                               :program-args (rest command)
                               :directory (clution--system.startup-dir system)
                               :init (lambda (port-filename coding-system)
                                       (format "(progn %s %S)\n\n"
                                               (slime-init-command port-filename coding-system)
                                               (clution--repl-form system))))))
            (if clution-intrusive-ui
                (save-window-excursion
                  (do-start))
              (do-start))))
         (slime-inferior-process (get-buffer-process slime-inferior-buffer))
         (prev-sentinel (process-sentinel slime-inferior-process))
         connected-hook)
    ;;Set up a hook for when we successfully connect, since we won't need to detect
    ;;a failed startup any more
    (setq connected-hook
          (lambda ()
            ;;Remove the hook
            (remove-hook 'slime-connected-hook connected-hook)
            ;;Restore the previous sentinel
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
                            (funcall prev-sentinel proc event)))

    ;;If we want to show the inferior buffer during startup, do so
    (when (and clution-intrusive-ui
               clution-show-inferior-start)
      (clution--display-in-repl-window slime-inferior-buffer))))

(defun clution--start-repl (system)
  "Start a new repl, according to style."
  (clution--append-output
   "\nclution-repl: starting with style '" (format "%s" clution-repl-style) "'\n")

  (setf *clution--current-op*
        (list
         :type 'clution-start-repl))

  (cl-ecase clution-repl-style
    (sly
     (clution--start-sly-repl system))
    (slime
     (clution--start-slime-repl system))))

(defun clution--restart-repl ()
  "Restart the currently active repl, according to style."
  (cl-ecase clution-repl-style
    (sly
     (when (sly-connected-p)
       (sly-quit-lisp-internal
        (sly-connection)
        (lambda (proc message)
          (funcall 'sly-quit-sentinel proc message)
          ;;TODO need to track which system was used to start the repl,
          ;;in the first place rather than the selected system
          (clution--start-repl (clution--clution.selected-system)))
        t)))
    (slime
     (when (slime-connected-p)
       (let ((sentinel
              (lambda (proc message)
                (cl-assert (process-status proc) 'closed)
                (let* ((inferior (slime-inferior-process proc)))
                  (when inferior (delete-process inferior))
                  (slime-net-close proc))
                (clution--start-repl (clution--clution.selected-system)))))
         (slime-quit-lisp-internal (slime-connection) sentinel t))))))

(defun clution--end-repl ()
  "Close the currently active repl, according to style."
  (cl-ecase clution-repl-style
    (sly
     (when (sly-connected-p)
       (sly-quit-lisp t)))
    (slime
     (when (slime-connected-p)
       (let ((sentinel
              (lambda (proc message)
                (cl-assert (process-status proc) 'closed)
                (let* ((inferior (slime-inferior-process proc)))
                  (when inferior (delete-process inferior))
                  (slime-net-close proc))

                (when (featurep 'slime-repl)
                  (slime-kill-all-buffers)))))
         (slime-quit-lisp-internal (slime-connection) sentinel t))))))

(defun clution--repl-start-failed ()
  "Called when the clution repl fails to start."
  (clution--append-output "\nclution-repl: repl failed to start\n")
  (when clution-intrusive-ui
    (when (window-live-p *clution--repl-window*)
      (delete-window *clution--repl-window*))
    (setf *clution--repl-window* nil))

  (run-hooks 'clution-repl-start-failed-hook)
  (setf *clution--current-op* nil))

(defun clution--repl-started ()
  "Called when the clution repl successfully starts."
  (clution--append-output "\nclution-repl: repl started\n")
  (run-hooks 'clution-repl-started-hook)
  (setf *clution--current-op* nil)
  (setf *clution--repl-active* t))

(defun clution--repl-exited ()
  "Called when the clution repl exits."
  (clution--append-output "\nclution-repl: repl exited\n")
  (when clution-intrusive-ui
    (when (window-live-p *clution--repl-window*)
      (delete-window *clution--repl-window*))
    (setf *clution--repl-window* nil))
  (run-hooks 'clution-repl-exited-hook)
  (setf *clution--repl-active* nil))

(defun clution--find-file-hook ()
  "Hook for `find-file-hook' which will open .clu or .asd files as current
clutions if `clution-auto-open' is enabled, and there is not already one active."
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
     ((string-match-p "^\\.clu$\\|^\\.asd$" (file-name-extension path t))
      (clution-open path)))))

(defun clution--file-watch-callback (event)
  "Callback whenever the current clution's file changes.
See `file-notify-add-watch'"
  (cl-destructuring-bind (descriptor action file &optional file1)
      event
    (cl-case action
      (created)
      (deleted
       (clution-close))
      (changed
       ;; (message "clution: Reloading clution (changed %S)" file)
       (clution--unwatch-systems *clution--current-clution*)
       (setf *clution--current-clution* (clution--parse-file file))
       (clution--watch-systems *clution--current-clution*)
       (clution--sync-buffers *clution--current-clution*))
      (renamed
       ;; (message "clution: Reloading clution (rename to %S)" file1)
       (clution--unwatch-systems *clution--current-clution*)
       (setf *clution--current-clution* (clution--parse-file file1))
       (clution--watch-systems *clution--current-clution*)
       (clution--sync-buffers *clution--current-clution*))
      (attribute-changed)
      (stopped))))

(defun clution--system-file-watch-callback (event)
  "Callback whenever one of the current clution's system files change.
See `file-notify-add-watch'"
  (cl-destructuring-bind (descriptor action file &optional file1)
      event
    (cl-case action
      (created)
      (deleted
       (let ((system (car (cl-rassoc descriptor *clution--system-watches* :test 'equal))))
         (unless system
           (error "clution: could not find system for descriptor '%s' (%s)" descriptor file))
         ;; (message "clution: Unloading system '%s' (deleted %S)" (clution--system.name system) file)
         (setf (getf system :query-node) nil)
         (clution--refresh-clutex)))
      (changed
       (let ((system (car (cl-rassoc descriptor *clution--system-watches* :test 'equal))))
         (unless system
           (error "clution: could not find system for descriptor '%s' (%s)" descriptor file))
         ;; (message "clution: Reloading system '%s' (changed %S)" (clution--system.name system) file)
         (clution--update-system-query system)
         (clution--refresh-clutex)))
      (renamed)
      (attribute-changed)
      (stopped))))

;;; System templates

(defun clution--insert-system-template (name dir version description author license static-files files depends-on)
  (insert
   (format "(defsystem #:%s
  :version \"%s\"
  :description \"%s\"
  :author \"%s\"
  :license \"%s\"
  :serial t
"
           name
           version
           description
           author
           license))
  (insert
   "  :components
  (")
  (let ((first t))
    (dolist (static-file static-files)
      (if first
          (setq first nil)
        (insert "\n   "))
      (insert (format "(:static-file \"%s\")" static-file)))
    (dolist (file files)
      (if first
          (setq first nil)
        (insert "\n   "))
      (insert (format "(:file \"%s\")" file))))
  (insert ")
")

  (insert
   "  :depends-on
  (")
  (let ((first t))
    (dolist (dep depends-on)
      (if first
          (setq first nil)
        (insert "\n  "))
      (insert (format "#:%s" dep))))
  (insert "))
"))

(defun clution--executable-system-template (path)
  (let ((name (file-name-base path))
        (dir (file-name-directory path))
        (version "0.0.0")
        (description "")
        (author (or (and user-full-name user-mail-address
                         (format "%s <%s>" user-full-name user-mail-address))
                    user-full-name
                    ""))
        (license (cdr (first *clution--licenses-alist*))))
    ;;Make the asd file
    (with-temp-file path
      (clution--insert-system-template name dir version description author license (list "qlfile") (list "package" "main") '("alexandria")))
    ;;Make the qlfile
    (with-temp-file (expand-file-name "qlfile" dir)
      (insert "ql alexandria :latest
"))

    ;;Make the package.lisp
    (let ((package-path (expand-file-name "package.lisp" dir)))
      (with-temp-file package-path
        (insert
         "(in-package #:cl-user)\n\n"
         (format "(defpackage #:%s
  (:use #:alexandria #:cl)
  (:export
    #:main))\n"
                 name))))

    ;;Make the main .lisp
    (let ((main-path (expand-file-name "main.lisp" dir)))
      (with-temp-file main-path
        (insert
         (format "(in-package #:%s)\n\n" name)
         (format "(defun main (&rest args)
  0)
"))))))

(defun clution--script-system-template (path)
  ;;Equivalent to executable
  (clution--executable-system-template path))

(defun clution--library-system-template (path)
  (let ((name (file-name-base path))
        (dir (file-name-directory path))
        (version "0.0.0")
        (description "")
        (author (or (and user-full-name user-mail-address
                         (format "%s <%s>" user-full-name user-mail-address))
                    user-full-name
                    ""))
        (license (cdr (first *clution--licenses-alist*))))
    ;;Make the asd file
    (with-temp-file path
      (clution--insert-system-template name dir version description author license (list "qlfile") (list "package" name) '("alexandria")))
    ;;Make the qlfile
    (with-temp-file (expand-file-name "qlfile" dir)
      (insert "ql alexandria :latest"))
    ;;Make the package.lisp
    (let ((package-path (expand-file-name "package.lisp" dir)))
      (with-temp-file package-path
        (insert
         "(in-package #:cl-user)\n\n"
         (format "(defpackage #:%s
  (:use #:alexandria #:cl)
  (:export))\n"
                 name))))

    ;;Make the main .lisp
    (let ((main-path (expand-file-name (concat name ".lisp") dir)))
      (with-temp-file main-path
        (insert
         (format "(in-package #:%s)\n\n" name))))))

(defun clution--open-clu (clu-path)
  "Opens the clu file pointed to by `clu-path' and makes it current."
  (when *clution--current-clution*
    (clution-close))

  (let ((clution (clution--parse-file clu-path)))
    ;;Parse the clution file and set it as current
    (let ((clu-dir (clution--clution.clu-dir clution)))
      ;;Make sure the clu directory exists
      (unless (file-exists-p clu-dir)
        (make-directory clu-dir t)
        (when (eq system-type 'windows-nt)
          ;;Set it hidden on Windows
          (clution--set-file-hidden-flag clu-dir t))))

    (setf *clution--current-clution* clution)
    (setf *clution--current-watch*
          (file-notify-add-watch clu-path '(change) 'clution--file-watch-callback))
    (clution--watch-systems clution)
    (clution--sync-buffers clution)
    (when clution-intrusive-ui
      (clution-open-output)
      (clution-open-clutex))

    (run-hooks 'clution-open-hook)))

(defun clution--enable ()
  (clution--cl-clution-start)
  (add-hook 'find-file-hook 'clution--find-file-hook))

(defun clution--disable ()
  (clution-close)
  (remove-hook 'find-file-hook 'clution--find-file-hook)
  (clution--cl-clution-stop))

;;;; Public interface

;;; Customization

(defgroup clution nil
  "Options for clution."
  :prefix "clution-"
  :group 'applications)

(defcustom clution-publish-alist '((:executable . clution--do-exe-publish)
                                   (:script . clution--do-script-publish))
  "An alist of publish target handlers."
  :type '(alist :key-type symbol :value-type function)
  :group 'clution)

(defcustom clution-system-template-alist '((:executable . clution--executable-system-template)
                                           (:script . clution--script-system-template)
                                           (:library . clution--library-system-template))
  "An alist of publish target handlers."
  :type '(alist :key-type symbol :value-type function)
  :group 'clution)

(defcustom clution-frontend 'raw
  "The frontend to use as default for clution."
  :type '(choice (const :tag "Use clution-backend directly" raw)
                 (const :tag "Roswell" roswell))
  :group 'clution)

(defcustom clution-backend 'sbcl
  "The backend to use as default for clution."
  :type '(choice (const :tag "sbcl" sbcl)
                 (const :tag "ccl" ccl))
  :group 'clution)

(defcustom clution-run-style 'term
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

(defcustom clution-auto-open 'nil
  "When enabled, clution will automatically open when visiting a .clu or .asd file."
  :type 'boolean
  :group 'clution)

(defcustom clution-intrusive-ui 't
  "When enabled, clution will automatically open the clutex and output buffers when opening a clution."
  :type 'boolean
  :group 'clution)

(defcustom clution-app-data-dir 'auto
  "Directory to use for clution application data, such as automatically
generated clution files."
  :type '(choice (const :tag "Automatically determine by following standard conventions for the platform." auto)
                 (directory :tag "Use a custom directory"))
  :group 'clution)

(defcustom clution-sbcl-path 'auto
  "Path to SBCL."
  :type '(choice (const :tag "Automatically find SBCL in PATH and SBCL_HOME." auto)
                 (file :must-match t :tag "Use the specified path"))
  :group 'clution)

(defcustom clution-ccl-path 'auto
  "Path to CCL."
  :type '(choice (const :tag "Automatically find CCL in PATH and CCL_DEFAULT_DIRECTORY" auto)
                 (file :must-match t :tag "Use the specified path"))
  :group 'clution)

(defcustom clution-ros-path 'auto
  "Path to roswell."
  :type '(choice (const :tag "Automatically find ros in PATH." auto)
                 (file :must-match t :tag "Use the specified path"))
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

;;
;; Faces
;;

(defface clution-clutex-clution-face
  '((t                   (:inherit dired-header)))
  "*Face used for the clution in clutex buffer."
  :group 'clutex :group 'font-lock-highlighting-faces)

(defface clution-clutex-system-face
  '((t                   (:inherit dired-directory)))
  "*Face used for systems in clutex buffer."
  :group 'clutex :group 'font-lock-highlighting-faces)

(defface clution-clutex-selected-system-face
  '((t                   (:inherit dired-marked)))
  "*Face used for the selected system in clutex buffer."
  :group 'clutex :group 'font-lock-highlighting-faces)

(defface clution-clutex-dependencies-face
  '((t                   (:inherit dired-header)))
  "*Face used for dependencies in clutex buffer."
  :group 'clutex :group 'font-lock-highlighting-faces)

(defface clution-clutex-dir-face
  '((t                   (:inherit dired-directory)))
  "*Face used for directories in clutex buffer."
  :group 'clutex :group 'font-lock-highlighting-faces)

(defface clution-clutex-file-face
  '((t                   (:inherit dired-perm-write)))
  "*Face used for files in clutex buffer."
  :group 'clutex :group 'font-lock-highlighting-faces)

;;; Modes and maps

(defvar clution-mode-map (make-sparse-keymap))
(let ((map clution-mode-map))
  (define-key map (kbd "C-S-b") 'clution-build)
  (define-key map (kbd "<f5>") 'clution-repl)
  (define-key map (kbd "S-<f5>") 'clution-end-repl)
  (define-key map (kbd "C-<f5>") 'clution-run)
  (define-key map (kbd "C-S-<f5>") 'clution-maybe-restart-repl)
  (define-key map (kbd "<f8>") 'clution-toggle-clutex)
  map)

;;;###autoload
(define-minor-mode clution-mode
  "minor mode for editing a clution project."
  nil nil nil
  :global t
  :after-hook
  (progn
    (cond
     (clution-mode
      (clution--enable))
     (t
      (clution--disable)))))

;;;###autoload
(define-derived-mode clution-file-mode lisp-mode
  "clution-file"
  "Major mode for editing a clution project file.")

;;;###autoload
(define-derived-mode cuo-file-mode lisp-mode
  "cuo-file"
  "Major mode for editing a clution user options file")

(defvar clution-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'clution-close-output)
    (define-key map (kbd "Q") 'clution-close)
    map))

;;;###autoload
(define-derived-mode clution-output-mode compilation-mode
  "clution-output"
  "Mode for the clution output buffer"
  (setq buffer-read-only t
        truncate-lines -1))

(defvar clutex-mode-map (make-sparse-keymap))
(let ((map clutex-mode-map))
  (define-key map (kbd "q") 'clution-close-clutex)
  (define-key map (kbd "Q") 'clution-close)
  (define-key map (kbd "n") 'clution--next-line-button)
  (define-key map (kbd "p") 'clution--prev-line-button)
  (define-key map (kbd "A") 'clution--clutex-invoke-line)
  (define-key map (kbd "D") 'clution--clutex-invoke-line)
  (define-key map (kbd "N") 'clution--clutex-invoke-line)
  (define-key map (kbd "C-S-N") 'clution--clutex-invoke-line)
  (define-key map (kbd "TAB") 'clution--clutex-invoke-line)
  (define-key map (kbd "RET") 'clution--clutex-invoke-line)
  (define-key map (kbd "<delete>") 'clution--clutex-invoke-line)
  (define-key map (kbd "S-<delete>") 'clution--clutex-invoke-line)
  map)

;;;###autoload
(define-derived-mode clutex-mode special-mode "ClutexMode"
  "A major mode for displaying the directory tree in a clution."
  (setq indent-tabs-mode nil
        buffer-read-only t
        truncate-lines -1))

;;; Functions

;;;###autoload
(defun clution-repl (system)
  "Activate a repl if there isn't one already active."
  (interactive
   (list
    (cond
     ((not *clution--current-clution*)
      (error "clution: no clution open"))
     (t
      (clution--clution.selected-system)))))
  (cond
   (*clution--current-op*
    (error "clution: busy doing op: '%s'" (cl-getf *clution--current-op* :type)))
   (*clution--repl-active*
    (error "clution: repl already active"))
   (t
    (clution-build
     (list system)
     (lambda ()
       (clution--clear-output)
       (clution--append-output
        "Starting repl for: '" (clution--system.name system)
        "'\n\n")
       (clution--start-repl system))))))

;;;###autoload
(defun clution-end-repl ()
  "End the current repl if it's active."
  (interactive)
  (cond
   ((not *clution--current-clution*)
    (error "clution: no clution open"))
   (*clution--current-op*
    (error "clution: busy doing op: '%s'" (cl-getf *clution--current-op* :type)))
   (*clution--repl-active*
    (clution--end-repl))
   (t
    (error "clution: no repl active"))))

;;;###autoload
(defun clution-maybe-restart-repl ()
  "Restart the repl if it's already active, or start a new one if it isn't."
  (interactive)
  (cond
   ((not *clution--current-clution*)
    (error "clution: no clution open"))
   (*clution--current-op*
    (error "clution: busy doing op: '%s'" (cl-getf *clution--current-op* :type)))
   (*clution--repl-active*
    (clution--clear-output)
    (clution--append-output
     "Retarting repl for: '" (clution--clution.name)
     "'\n\n")
    (clution--restart-repl))
   (t
    (clution-repl (clution--clution.selected-system)))))

;;;###autoload
(defun clution-qlfile-sync (clution &optional cont)
  "Install & update qlfile packages for the current clution."
  (interactive
   (list
    (cond
     ((not *clution--current-clution*)
      (error "clution: no clution open"))
     (t
      *clution--current-clution*))
    nil))
  (cond
   (*clution--current-op*
    (error "clution: busy doing op: '%s'" (cl-getf *clution--current-op* :type)))
   (t
    (clution--clear-output)
    (clution--do-qlfile-sync clution cont))))

;;;###autoload
(defun clution-build (systems &optional cont)
  "Perform a 'build' operation on each system in the clution."
  (interactive
   (list
    (cond
     ((not *clution--current-clution*)
      (error "clution: no clution open"))
     (t
      (clution--clution.systems)))
    nil))
  (cond
   (*clution--current-op*
    (error "clution: busy doing op: '%s'" (cl-getf *clution--current-op* :type)))
   (*clution--repl-active*
    (clution--clear-output)
    (clution--kickoff-build-in-repl systems))
   ((null systems)
    (clution--clear-output)
    (clution--append-output
     "No systems to build"
     "'\n\n"))
   (t
    (let ((clution (clution--system.clution (first systems))))
      (cond
       ((not (clution--clution.qlfile-libs-up-to-date clution))
        (clution-qlfile-sync
         clution
         (lambda () (clution--do-build systems cont))))
       (t
        (clution--clear-output)
        (clution--do-build systems cont)))))))

;;;###autoload
(defun clution-run (system)
  "Perform a 'run' operation on the currently selected system in the clution"
  (interactive
   (list
    (cond
     ((not *clution--current-clution*)
      (error "clution: no clution open"))
     (t
      (clution--clution.selected-system)))))
  (cond
   (*clution--current-op*
    (error "clution: busy doing op: '%s'" (cl-getf *clution--current-op* :type)))
   (*clution--repl-active*
    ;;Kill repl and try again
    (clution--end-repl)
    (clution-run system))
   (t
    (clution-build
     (list system)
     (lambda ()
       (clution--do-run system))))))

;;;###autoload
(defun clution-publish (system)
  "Perform a 'run' operation on the currently selected system in the clution"
  (interactive
   (list
    (cond
     ((not *clution--current-clution*)
      (error "clution: no clution open"))
     (t
      (clution--clution.selected-system)))))
  (cond
   (*clution--current-op*
    (error "clution: busy doing op: '%s'" (cl-getf *clution--current-op* :type)))
   (t
    (clution-build
     (list system)
     (lambda ()
       (clution--do-publish system))))))

;;;###autoload
(defun clution-clean (systems &optional cont)
  "Perform a 'clean' operation on the current clution."
  (interactive
   (list
    (cond
     ((not *clution--current-clution*)
      (error "clution: no clution open"))
     (t
      (clution--clution.systems)))
    nil))
  (cond
   (*clution--current-op*
    (error "clution: busy doing op: '%s'" (cl-getf *clution--current-op* :type)))
   ((null systems)
    (clution--clear-output)
    (clution--append-output
     "No systems to clean"
     "'\n\n"))
   (t
    (clution--clear-output)
    (let ((clution (clution--system.clution (first systems))))
      (clution--append-output
       "Clean starting: '" (clution--clution.name clution)
       "'\n\n")
      (clution--do-clean
       systems
       (lambda ()
         (clution--append-output
          "Clean complete: '" (clution--clution.name clution)
          "'\n")
         (when cont (funcall cont))))))))

;;;###autoload
(defun clution-create-clution (path &optional open)
  "Create a new clution file at `path'"
  (interactive
   (list (clution--read-new-file-name "create new clution at: ")
         t))
  (unless (string= (file-name-extension path t) ".clu")
    (setf path (concat path ".clu")))

  (setf path (expand-file-name path))

  (when (or (not (file-exists-p path))
            (y-or-n-p (format "file '%s' already exists. overwrite?" path)))
    ;;Create the clution file
    (clution--cl-clution-eval
     `(create-clu ',path))

    (when open
      (clution-open path)
      (select-window (clution--clutex-open-file path)))))

(defvar *clution--licenses-alist*
  '((mit . "MIT")
    (zlib . "zlib/libpng")
    (libpng . "zlib/libpng")
    (zlib/libpng . "zlib/libpng")
    (bsd . "BSD-2-Clause")
    (bsd-2 . "BSD-2-Clause")
    (bsd-2-clause . "BSD-2-Clause")
    (bsd-3 . "BSD-3-Clause")
    (bsd-3-clause . "BSD-3-Clause")
    (cc0 . "CC0")))

(defvar clution--system-type-history nil)

;;;###autoload
(defun clution-create-system (clution parent-dir &optional open)
  "Create a new clution file at `path'"
  (interactive
   (list
    *clution--current-clution*
    nil
    t))
  (clution--create-system clution parent-dir))

(defun clution-create-directory (clution parent-dir)
  (interactive
   (list
    (cond
     ((not *clution--current-clution*)
      (error "clution: no clution open"))
     (t *clution--current-clution*))
    nil))
  (clution--add-directory clution parent-dir))

;;;###autoload
(defun clution-add-system (clution parent-dir)
  (interactive
   (list
    (cond
     ((not *clution--current-clution*)
      (error "clution: no clution open"))
     (t *clution--current-clution*))
    nil))
  (clution--add-system clution parent-dir))

;;;###autoload
(defun clution-open (path type)
  "Opens `path' and sets it as the current clution.
If the path points to a clution file, open it.
If the path points to an asd file, open the autogenerated clution for it."
  (interactive
   (list (clution--read-file-name "clution/asd to open: " nil nil t)
         nil))

  (let* ((path (expand-file-name path))
         (ext (file-name-extension path t)))
    (cond
     ((string= ext ".clu")
      ;;Just open the clu file
      (clution--open-clu path))
     ((string= ext ".asd")
      ;;Figure out where the autogenerated clution is
      (let* ((asd-clution-path (clution--asd-clution-path path)))
        ;;Make sure the autogenerated clution exists
        (unless (file-exists-p asd-clution-path)
          (unless type
            (setf type (clution--read-system-type "System type: ")))
          ;;Create the clution file
          (clution--cl-clution-eval
           `(create-clu ',asd-clution-path))
          ;;Add the system to it
          (clution--cl-clution-eval
           `(add-clu-system ',asd-clution-path nil ',path ',type)))

        ;;Open the autogenerated clution file
        (clution--open-clu asd-clution-path)))
     (t
      (error "clution: unrecognized file '%s'" path)))))

;;;###autoload
(defun clution-close ()
  "Close the currently open clution, ending a repl if it is active."
  (interactive)
  (when *clution--current-clution*
    (when *clution--repl-active*
      (clution-end-repl))

    (when *clution--current-op*
      (warn "clution: closed in the middle of op: '%s'" (getf *clution--current-op* :type))
      (setf *clution--current-op* nil))

    (clution--sync-buffers nil)

    (clution--unwatch-systems *clution--current-clution*)

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

;;;###autoload
(defun clution-open-output ()
  "Opens the clution output window."
  (interactive)
  (unless (window-live-p *clution--output-window*)
    (let ((buffer (clution--output-buffer t)))
      (clution--sync-output *clution--current-clution* buffer)
      (setf *clution--output-window*
            (display-buffer buffer '(clution-output-default-display-fn))))
    (clution--init-output-window *clution--output-window*)))

;;;###autoload
(defun clution-close-output ()
  "Closes the clution output window."
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

;;;###autoload
(defun clution-toggle-clutex ()
  (interactive)
  (if (window-live-p *clution--clutex-window*)
      (clution-close-clutex)
    (clution-open-clutex)))

;;;###autoload
(defun clution-open-clutex ()
  (interactive)
  (unless (window-live-p *clution--clutex-window*)
    (let ((buffer (clution--clutex-buffer t)))
      (clution--sync-clutex *clution--current-clution* buffer)
      (setf *clution--clutex-window*
            (display-buffer buffer '(clution-clutex-default-display-fn))))
    (clution--init-clutex-window *clution--clutex-window*)))

;;;###autoload
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
;;; clution.el ends here
