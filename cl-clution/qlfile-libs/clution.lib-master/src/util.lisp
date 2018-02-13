;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:%clution.lib)

(defun %whitespacep (char)
  "Determine if `char' is a whitespace character"
  (check-type char character)
  ;;Abuse behavior of peek-char so we can use the
  ;;implementation's notion of what whitespace is
  (with-input-from-string (s (string char))
    (not (peek-char t s nil nil))))

(defun %terminating-p (char)
  "Determine if `char' is a 'terminating' character (delimits a token).
That is, if it is either a macro character that is terminatting, or if it is whitespace."
  (check-type char character)
  (multiple-value-bind (fn non-terminating-p)
      (get-macro-character char)
    (if fn
        (not non-terminating-p)
        (%whitespacep char))))

(defun %cell-before (item list &key (test #'eql))
  (cond
    ((funcall test item (car list))
     nil)
    (t
     (loop
       :for cell := list :then (cdr cell)
       :while (cdr cell)
       :when (funcall test item (cadr cell))
         :return cell))))

(defun %cell-after (item list &key (test #'eql))
  (cdr (member item list :test test)))

(defun %item-before (item list &key default (test #'eql))
  (if-let ((cell (%cell-before item list :test test)))
    (car cell)
    default))

(defun %item-after (item list &key default (test #'eql))
  (if-let ((cell (%cell-after item list :test test)))
    (car cell)
    default))

(deftype %eol-style ()
  '(member :old-mac :unix :windows))

(defvar *%eol-sequences*
  (list
   (cons :windows '(#\Return #\Newline))
   (cons :unix '(#\Newline))
   (cons :old-mac '(#\Return))))

(defvar *%eol-style* :unix)

(defun %guess-eol-style (file-path &optional (default *%eol-style*))
  (let ((scores
          (list
           (cons :windows 0)
           (cons :unix 0)
           (cons :old-mac 0))))
    (with-input-from-file (stream file-path :external-format :utf-8)
      (loop
        :for c := (read-char stream nil nil)
        :while c
        :do
           (switch (c)
             (#\Return
              (switch ((read-char stream nil nil))
                (#\Newline
                 (incf (cdr (assoc :windows scores))))
                (t
                 (incf (cdr (assoc :old-mac scores))))))
             (#\Newline
              (incf (cdr (assoc :unix scores)))))))

    (setf scores (stable-sort scores #'> :key #'cdr))
    (cond
      ((every (lambda (cell) (zerop (cdr cell))) scores)
       ;;All zero
       default)
      (t ;;Possibly mixed, but return most popular
       (caar scores)))))

(defun %eol-sequence (&optional (style *%eol-style*))
  (check-type style %eol-style)
  (cdr (assoc style *%eol-sequences*)))

(defun %read-from-file (filename &rest args
                        &key
                          (eof-error-p nil)
                          (eof-value nil)
                          (if-does-not-exist nil if-does-not-exist-p)
                          (direction nil direction-p)
                        &allow-other-keys)
  (declare (ignore if-does-not-exist direction))
  (when if-does-not-exist-p
    (error "Can't specify :IF-DOES-NOT-EXIST for READ-FROM-FILE."))
  (when direction-p
    (error "Can't specify :DIRECTION for READ-FROM-FILE."))

  (let ((file-stream (apply #'open filename :direction :input :if-does-not-exist nil args)))
    (cond
      (file-stream
       (unwind-protect
            (read file-stream eof-error-p eof-value)
         (close file-stream)))
      (t
       (if eof-error-p
           (error 'end-of-file :stream nil)
           eof-value)))))

(defun %pathname-equal (p1 p2)
  "Tests if `p1' and `p2' are reasonably equal."
  (#+windows string-equal
   #-windows string=
   (namestring p1)
   (namestring p2)))

(defun %pathname-as-directory (pathname)
  "This function returns a pathname representing `pathname' in a form that the
operating system will interpret as the name of a directory (a directory name).
On most systems, this means appending a slash to the string (if it does not already end in one)."
  (setf pathname (pathname pathname))
  (cond
    ((uiop:directory-pathname-p pathname)
     pathname)
    (t
     (let* ((name.type
              (cond
                ((and (pathname-name pathname)
                      (pathname-type pathname))
                 (format nil "~A.~A" (pathname-name pathname) (pathname-type pathname)))
                ((pathname-name pathname)
                 (pathname-name pathname))
                ((pathname-type pathname)
                 (pathname-type pathname))
                (t
                 nil)))
            (dir-list
              (if-let ((lst (uiop:normalize-pathname-directory-component
                             (pathname-directory pathname))))
                lst
                '(:relative))))
       (make-pathname
        :host (pathname-host pathname)
        :device (pathname-device pathname)
        :directory
        (uiop:denormalize-pathname-directory-component
         (append
          dir-list
          (if name.type (list name.type) nil)))
        :name nil
        :type nil
        :version (pathname-version pathname))))))

(defun %resolve-directives (pathname
                            &aux
                              (device (pathname-device pathname))
                              (host (pathname-host pathname)))
  "Attempts to resolve special directives of `pathname'.
In the directory component:
:UP, :BACK, and \"..\" will be interpreted as 'eliminating' the next directory up.
\".\" will be interpreted as a no-op and 'eliminated'
:HOME and \"~\" will resolved to `user-homedir-pathname', using the same `host' component as `pathname'

In the name component:
\".\" will be interpreted as the name of the directory, as per `.

eg:
/foo/bar/baz/../ => /foo/bar/

/foo/bar/baz/. => /foo/bar/baz
/foo/bar/baz/./ => /foo/bar/baz/

Note that the parent directory of root is itself. Therefore
/../../../../ => /

When `pathname' is relative, any such directives that cannot be resolved will be left intact
foo/../../bar/ => ../bar/
"
  (labels ((recurse (components)
             (cond
               ((null components) nil)
               ((member (car components) '(:up :back "..") :test #'equal)
                (let ((res (recurse (cdr components))))
                  (cond
                    ((null res)
                     (list (car components) :relative))
                    ((member (car res) '(:up :back ".." :relative) :test #'equal)
                     ;;keep it and ourselves
                     (cons (car components) res))
                    ((eq (car res) :absolute)
                     (list :absolute))
                    (t
                     ;;skip it
                     (cdr res)))))
               ((member (car components) '(".") :test #'equal)
                 ;;skip it
                (recurse (cdr components)))
               ((member (car components) '(:home "~") :test #'equal)
                (let ((homedir (user-homedir-pathname host)))
                  (setf device (pathname-device homedir)
                        host (pathname-host homedir))
                  (reverse (uiop:normalize-pathname-directory-component
                            (pathname-directory homedir)))))
               (t
                (cons (car components) (recurse (cdr components)))))))
    (let* ((directory (recurse
                       (reverse
                        (uiop:normalize-pathname-directory-component
                         (pathname-directory pathname)))))
           (name (pathname-name pathname)))
      (when (and name (string= name "."))
        ;;Name becomes name of directory, instead
        (cond
          ((cdr directory)
           (setf name (pop directory)))
          (t
           (setf name nil))))
      (make-pathname
       :directory (uiop:normalize-pathname-directory-component (nreverse directory))
       :device device
       :host host
       :name name
       :defaults pathname))))

(defun %expand-pathname (pathname &optional (base *default-pathname-defaults*))
  (%resolve-directives
   (uiop:merge-pathnames* pathname (uiop:merge-pathnames* (%pathname-as-directory base)))))

(defun %relative-pathname (pathname &optional (base *default-pathname-defaults*))
  "This function tries to return a relative name that is equivalent to `pathname' assuming the result will be interpreted relative to `base'.
On some operating systems, an absolute file name begins with a device name. On such systems, filename has no relative equivalent based on directory if they start with two different device names. In this case, file-relative-name returns filename in absolute form."
  (setf pathname (%expand-pathname pathname)
        base (%pathname-as-directory (%expand-pathname base)))
  (let (subpath)
    (cond
      ((not (equalp (pathname-device pathname) (pathname-device base)))
       pathname)
      ((%pathname-equal pathname base) #P"")
      ((and (setf subpath (pathname (enough-namestring pathname base)))
            (uiop:relative-pathname-p subpath))
       subpath)
      (t
       (let ((result-dir (list :relative)))
         (labels ((climb (base)
                    (cond
                      ((%pathname-equal pathname base)
                       (make-pathname
                        :device nil
                        :host nil
                        :directory (uiop:denormalize-pathname-directory-component
                                    (nreverse result-dir))
                        :defaults base))
                      ((and (setf subpath (pathname (enough-namestring pathname base)))
                            (uiop:relative-pathname-p subpath))
                       (make-pathname
                        :device nil
                        :host nil
                        :directory
                        (uiop:denormalize-pathname-directory-component
                         (append
                          (nreverse result-dir)
                          (rest
                           (uiop:normalize-pathname-directory-component
                            (pathname-directory subpath)))))
                        :defaults subpath))
                      (t
                       (push :up result-dir)
                       (climb
                        (make-pathname
                         :directory
                         (uiop:denormalize-pathname-directory-component
                          (butlast
                           (uiop:normalize-pathname-directory-component
                            (pathname-directory base))))
                         :defaults base))))))
           (climb base)))))))

(defun %directory-pathname (pathname)
  "This function returns a string representing dirname in a form that the operating system will interpret as the name of a file (a directory file name). On most systems, this means removing the final slash (or backslash) from the string."
  (setf pathname (pathname pathname))
  (cond
    ((uiop:file-pathname-p pathname)
     pathname)
    ((uiop:directory-pathname-p pathname)
     (let* ((dir (uiop:normalize-pathname-directory-component
                  (pathname-directory pathname)))
            (name (lastcar dir)))
       (cond
         ((stringp name)
          (uiop:merge-pathnames*
           (pathname name)
           (make-pathname
            :host (pathname-host pathname)
            :device (pathname-device pathname)
            :directory (uiop:denormalize-pathname-directory-component (butlast dir))
            :name nil
            :type nil
            :version nil)))
         (t
          pathname))))
    (t
     pathname)))

(defun %directory-name (pathname)
  "Get the name of the directory designated by `pathname'."
  (setf pathname (%pathname-as-directory pathname))
  (let ((name (lastcar (uiop:normalize-pathname-directory-component (pathname-directory pathname)))))
    (typecase name
      (string name)
      (t ""))))

(defun %ensure-relative-pathname (pathname &optional (base *default-pathname-defaults*))
  (let ((ret (%relative-pathname pathname base)))
    (when (uiop:absolute-pathname-p pathname)
      (error "could not make pathname relative: ~A" pathname))
    ret))

(defun %path-combine (path &rest rest-paths)
  (loop
    :with paths := (reverse (cons path rest-paths))
    :with result := (pathname (first paths))
    :for path :in  (rest paths)
    :do (setf result (uiop:merge-pathnames* result (%pathname-as-directory path)))
    :finally (return result)))

(defun %copy-directory (pathname new-name &key copy-contents)
  (let* ((dir-name (%pathname-as-directory pathname))
         (dir-new-name (%pathname-as-directory
                        (if copy-contents
                            new-name
                            (%path-combine new-name (%directory-name dir-name))))))
    (flet ((collectp (d)
             (declare (ignore d))
             t)
           (recursep (d)
             (declare (ignore d))
             t)
           (collector (d)
             (let* ((rel-dir-name (%relative-pathname d dir-name))
                    (rel-dir-new-name (%pathname-as-directory (%path-combine dir-new-name rel-dir-name))))
               (ensure-directories-exist rel-dir-new-name))
             (dolist (f (uiop:directory-files d))
               (let* ((rel-name (%relative-pathname f dir-name))
                      (rel-new-name (%path-combine dir-new-name rel-name)))
                 (copy-file f rel-new-name :finish-output t)))))
      (uiop:collect-sub*directories dir-name #'collectp #'recursep #'collector))
    (values)))

(defun %app-data-dir (app-name)
  "Directory for storing per-user per-application data files."
  (%pathname-as-directory
   #+windows
   (%expand-pathname
    "data"
    (%expand-pathname
     app-name
     (uiop:getenv "LOCALAPPDATA")))
   #-windows
   (%expand-pathname
    app-name
    (uiop:xdg-data-home))))

(defun %app-config-dir (app-name)
  "Directory for storing per-user per-application configuration files."
  (%pathname-as-directory
   #+windows
   (%expand-pathname
    "config"
    (%expand-pathname
     app-name
     (uiop:getenv "LOCALAPPDATA")))
   #-windows
   (%expand-pathname
    app-name
    (uiop:xdg-config-home))))

(defun %app-temp-dir (app-name)
  "Directory for storing per-application temporary files."
  (%pathname-as-directory
   (%expand-pathname
    app-name
    (uiop:temporary-directory))))
