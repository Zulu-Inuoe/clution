;;;asd-serializer - read/writer for asdf asd files
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:asd-serializer)

(defun %coerce-component-path (path)
  (etypecase path
    (null nil)
    (string-designator (list (string path)))
    (enumerable path)))

(defun %coerce-name-node (name)
  (etypecase name
    (symbol
     (make-instance 'sexp-symbol-node :name name :package :external))
    (string
     (make-instance 'sexp-string-node :string name))))

(defun %coerce-name-string (name)
  (etypecase name
    (sexp-symbol-node
     (symbol-node-name name))
    (sexp-string-node
     (string-node-string name))))

(defun %coerce-path-node (path)
  (etypecase path
    (symbol
     (make-instance 'sexp-symbol-node :name (symbol-name path)))
    (string
     (make-instance 'sexp-string-node :string path))
    (pathname
     ;;TODO
     )))

(defun %coerce-path-string (path &optional type)
  (etypecase path
    (sexp-symbol-node
     (namestring (make-pathname :type type :defaults (string-downcase (symbol-node-name path)))))
    (sexp-string-node
     (namestring (make-pathname :type type :defaults (string-node-string path))))
    ;;TODO need pathname handling
    ))

(defun %file-node-p (component)
  (and (%list-node-p component)
       (%symbol-node-p (sexp-list-nth component 0))
       (sexp-symbol-match (sexp-list-nth component 0) "FILE" :keyword)))

(defun %module-node-p (component)
  (and (%list-node-p component)
       (%symbol-node-p (sexp-list-nth component 0))
       (sexp-symbol-match (sexp-list-nth component 0) "MODULE" :keyword)))

(defun %node-indention-level (node
                             &aux
                               (parent (sexp-node-parent node)))
  (cond
    ((null parent)
     0)
    ((eq (first (children parent)) node)
     (1+ (%node-indention-level parent)))
    (t
     (loop
       :for prev-cell := (%cell-before node (children parent))
         :then (%cell-before prev-node (children parent))
       :for prev-node := (car prev-cell)
       :while (%whitespace-node-p prev-node)
       :sum (ecount (take-while (reverse (opaque-node-text prev-node)) (lambda (c) (char= c #\Space))))))))

(defun %make-indent-node (indent skip-newline-p &optional parent)
  (let ((prefix-whitespace
          (with-output-to-string (res)
            (unless skip-newline-p
              (format res "~%"))
            (format res "~A" (make-string indent :initial-element #\Space)))))
    (make-instance 'sexp-whitespace-node
                   :parent parent
                   :text prefix-whitespace)))

(defun %list-node-p (node)
  (typep node 'sexp-list-node))

(defun %whitespace-node-p (node)
  (typep node 'sexp-whitespace-node))

(defun %opaque-node-p (node)
  (typep node 'sexp-opaque-node))

(defun %symbol-node-p (node)
  (typep node 'sexp-symbol-node))

(defun %string-node-p (node)
  (typep node 'sexp-string-node))

(defun %system-node-p (node)
  (and (%list-node-p node)
       (when-let ((first-child (efirst (vchildren node))))
         (and (%symbol-node-p first-child)
              (sexp-symbol-match first-child "DEFSYSTEM")))))

(defun %opaque-node-ends-with-newline (node)
  (and (%opaque-node-p node)
       (ends-with #\Newline (opaque-node-text node))))

(defun %append-to-list-node (list-node new-node
                             &aux
                               (indent (+ (%node-indention-level list-node)
                                          (if (%system-node-p list-node) 2 1))))
  ;;Set the parent
  (setf (sexp-node-parent new-node) list-node)

  ;;Find the last non-whitespace child
  (let ((last-child (elast* (children list-node) (complement #'%whitespace-node-p))))
    (cond
      (last-child
       (let* ((cell (member last-child (children list-node))))
         (setf (cdr cell)
               (list
                (%make-indent-node indent (%opaque-node-ends-with-newline last-child) list-node)
                new-node))))
      (t
       ;;We are the first non-whitespace child
       (setf (children list-node) (list new-node))))))

(defun %create-file-component (name &key parent pathname)
  (let ((nodes ()))
    (push (make-instance 'sexp-symbol-node :name "FILE" :package :keyword) nodes)
    (push (make-instance 'sexp-whitespace-node :text " ") nodes)
    (push (%coerce-name-node name) nodes)

    (when pathname
      (push (make-instance 'sexp-whitespace-node :text " ") nodes)
      (push (make-instance 'sexp-symbol-node :name "PATHNAME" :package :keyword) nodes)
      (push (make-instance 'sexp-whitespace-node :text " ") nodes)
      (push (%coerce-path-node pathname)  nodes))

    (make-instance
     'sexp-list-node
     :parent parent
     :children (nreverse nodes))))

(defun %create-module-component (name &key parent pathname (serial nil serial-sup-p)
                                &aux
                                  (indent (if parent
                                              (+ (%node-indention-level parent)
                                                 (if (%system-node-p parent) 3 2))
                                              0)))
  (let ((nodes ()))
    (push (make-instance 'sexp-symbol-node :name "MODULE" :package :keyword) nodes)
    (push (make-instance 'sexp-whitespace-node :text " ") nodes)
    (push (%coerce-name-node name) nodes)

    (when pathname
      (push (%make-indent-node indent nil) nodes)
      (push (make-instance 'sexp-symbol-node :name "PATHNAME" :package :keyword) nodes)
      (push (make-instance 'sexp-whitespace-node :text " ") nodes)
      (push (%coerce-path-node pathname) nodes))

    (when serial-sup-p
      (push (%make-indent-node indent nil) nodes)
      (push (make-instance 'sexp-symbol-node :name "SERIAL" :package :keyword) nodes)
      (push (make-instance 'sexp-whitespace-node :text " ") nodes)
      (push (make-instance 'sexp-symbol-node :name (if serial "T" "NIL")) nodes))

    (push (%make-indent-node indent nil) nodes)
    (push (make-instance 'sexp-symbol-node :name "COMPONENTS" :package :keyword) nodes)
    (push (%make-indent-node indent nil) nodes)
    (push (make-instance 'sexp-list-node :children ()) nodes)

    (make-instance
     'sexp-list-node
     :parent parent
     :children (nreverse nodes))))

(defun component-name (component)
  (let ((name-prop
          (or
           (sexp-list-getf component "FILE")
           (sexp-list-getf component "MODULE"))))
    (unless (and name-prop (%string-node-p name-prop))
      (error "malformed component: '~A'" component))
    name-prop))

(defun component-pathname (component)
  (sexp-list-getf component "PATHNAME"))

(defun component-default-component-class (component)
  (sexp-list-getf component "DEFAULT-COMPONENT-CLASS"))

(defun component-perform (component)
  (sexp-list-getf component "PERFORM"))

(defun component-output-files (component)
  (sexp-list-getf component "OUTPUT-FILES"))

(defun component-operation-done-p (component)
  (sexp-list-getf component "OPERATION-DONE-P"))

(defun component-if-feature (component)
  (sexp-list-getf component "IF-FEATURE"))

(defun component-depends-on (component)
  (sexp-list-getf component "DEPENDS-ON"))

(defun component-in-order-to (component)
  (sexp-list-getf component "IN-ORDER-TO"))

(defun component-remove (component)
  (let* ((parent (sexp-node-parent component))
         (cell (member component (children parent))))
    ;;Delete any white space that follows it
    (loop :while (and (cdr cell) (%whitespace-node-p (cadr cell)))
          :do (setf (cdr cell) (cddr cell)))

    ;;If it's the last node in the parent, remove whitespace before it, too
    (when (null (cdr cell))
      (loop
        :for prev-cell := (%cell-before component (children  parent))
        :while (and prev-cell (%whitespace-node-p (car prev-cell)))
        :do (setf (children parent) (delete (car prev-cell) (children parent)))))

    ;;Delete it
    (setf (children parent) (delete component (children parent)))

    ;;If there is only whitespace left, then clear out all children
    (when (all (children parent) #'%whitespace-node-p)
      (setf (children parent) nil))))

(defun module-name (module)
  (component-name module))

(defun module-components (module)
  (sexp-list-getf module "COMPONENTS"))

(defun module-serial (module)
  (sexp-list-getf module "SERIAL"))

(defun module-ensure-components (module)
  (unless (module-components module)
    (%append-to-list-node module (make-instance 'sexp-symbol-node :name "COMPONENTS" :package :keyword))
    (%append-to-list-node module (make-instance 'sexp-list-node :children ())))
  (values))

(defun module-ensure-depends-on (module)
  (unless (component-depends-on module)
    (%append-to-list-node module (make-instance 'sexp-symbol-node :name "DEPENDS-ON" :package :keyword))
    (%append-to-list-node module (make-instance 'sexp-list-node :children ())))
  (values))

;;;; System accessors
(defun system-name (system)
  (sexp-list-nth system 1))

(defun system-long-name (system)
  (sexp-list-getf system "LONG-NAME"))

(defun system-version (system)
  (sexp-list-getf system "VERSION"))

(defun system-description (system)
  (sexp-list-getf system "DESCRIPTION"))

(defun system-long-description (system)
  (sexp-list-getf system "LONG-DESCRIPTION"))

(defun system-author (system)
  (sexp-list-getf system "AUTHOR"))

(defun system-license (system)
  (sexp-list-getf system "LICENSE"))

(defun system-pathname (system)
  (component-pathname system))

(defun system-serial (system)
  (module-serial system))

(defun system-components (system)
  (module-components system))

(defun system-depends-on (system)
  (component-depends-on system))

(defun component-plist (component dir)
  (let ((plist (list))
        (pathname
          (namestring
           (uiop:merge-pathnames*
            (or
             (%coerce-path-string
              (or
               (component-pathname component)
               (component-name component))
              (if (%file-node-p component) "lisp" nil)))
            dir))))
    (flet ((add-prop (name value)
             (push name plist)
             (push value plist)))
      (add-prop :name (%coerce-name-string (component-name component)))
      (add-prop :pathname pathname)
      (cond
        ((%file-node-p component)
         (add-prop :type :file))
        ((%module-node-p component)
         (add-prop :type :module)
         (when-let ((components (module-components component)))
           (add-prop :components
                     (-> (vchildren components)
                         (select (lambda (c) (component-plist c (uiop:pathname-directory-pathname pathname))))
                         (to-list)))))
        (t
         (error "malformed component"))))
    (nreverse plist)))

(defun system-plist (system path)
  (let ((plist (list))
        (pathname
          (namestring
           (or
            (when-let ((pathname (system-pathname system)))
              (uiop:merge-pathnames*
               (%coerce-path-string pathname)
               (uiop:pathname-directory-pathname path)))
            path))))
    (flet ((add-prop (name value)
             (push name plist)
             (push value plist)))
      (add-prop :name (%coerce-name-string (system-name system)))
      (add-prop :pathname pathname)
      (when-let ((components (system-components system)))
        (add-prop :components (-> (vchildren components)
                                  (select (lambda (c) (component-plist c (uiop:pathname-directory-pathname pathname))))
                                  (to-list))))
      (when-let ((depends-on (system-depends-on system)))
        (add-prop :depends-on (-> (vchildren depends-on)
                                  (select #'%coerce-name-string)
                                  (to-list)))))
    (nreverse plist)))

(defun system-ensure-components (system)
  (module-ensure-components system)
  (values))

(defun system-component-by-path (system component-path)
  (labels ((recurse (node path)
             (cond
               ((move-next path)
                (when-let* ((components (module-components node))
                            (child (efirst* (vchildren components)
                                            (lambda (child)
                                              (string= (string-node-string (component-name child))
                                                       (current path))))))
                   (recurse child path)))
               (t
                node))))
    (recurse system (get-enumerator component-path))))

(defun system-rename-component (system component-path new-name)
  (let ((component (system-component-by-path system component-path)))
    (unless component
      (error "component does not exist: '~A'" component-path))
    (when (component-pathname component)
      (error "cannot yet handle components with pathnames"))
    (let ((name-prop (component-name component)))
      (setf (string-node-string name-prop) new-name))))

(defun system-add-file-component (system module-path name &key pathname)
  (setf module-path (%coerce-component-path module-path))
  (let ((module (system-component-by-path system  module-path)))
    (unless module
      (error "module does not exist: '~A'" module-path))
    (module-ensure-components module)

    (let ((components (module-components module)))
      (%append-to-list-node components (%create-file-component name :pathname pathname)))))

(defun system-add-module-component (system module-path name &key pathname)
  (setf module-path (%coerce-component-path module-path))
  (let ((module (system-component-by-path system  module-path)))
    (unless module
      (error "module does not exist: '~A'" module-path))
    (module-ensure-components module)

    (let* ((components (module-components module)))
      (%append-to-list-node
       components
       (%create-module-component name :parent components :pathname pathname)))))

(defun system-remove-component (system component-path)
  (setf component-path (%coerce-component-path component-path))
  (let ((component (system-component-by-path system component-path)))
    (unless component
      (error "component does not exist: '~A'" component-path))
    (component-remove component)))

(defun system-add-depends-on (system module-path name)
  (setf module-path (%coerce-component-path module-path))
  (let ((module (system-component-by-path system  module-path)))
    (unless module
      (error "module does not exist: '~A'" module-path))
    (module-ensure-depends-on module)

    (let* ((depends-on (component-depends-on module)))
      (%append-to-list-node depends-on (%coerce-name-node name)))))

(defun system-remove-depends-on (system module-path name)
  (setf module-path (%coerce-component-path module-path))
  (let ((module (system-component-by-path system  module-path)))
    (unless module
      (error "module does not exist: '~A'" module-path))

    (when-let* ((depends-on (component-depends-on module))
                (node (efirst* (vchildren depends-on)
                               (lambda (node)
                                 (typecase node
                                   (sexp-symbol-node
                                    (sexp-symbol-match node name))
                                   (sexp-string-node
                                    (string= (string-node-string node) name)))))))
      (let* ((cell (member node (children depends-on))))
        ;;Delete any white space that follows it
        (loop :while (and (cdr cell) (%whitespace-node-p (cadr cell)))
              :do (setf (cdr cell) (cddr cell)))

        ;;If it's the last node in the parent, remove whitespace before it, too
        (when (null (cdr cell))
          (loop
            :for prev-cell := (%cell-before node (children depends-on))
            :while (and prev-cell (%whitespace-node-p (car prev-cell)))
            :do (setf (children depends-on) (delete (car prev-cell) (children depends-on)))))

        ;;Delete it
        (setf (children depends-on) (delete node (children depends-on)))

        ;;If there is only whitespace left, then clear out all children
        (when (all (children depends-on) #'%whitespace-node-p)
          (setf (children depends-on) nil))))))

(defun system-move-component-up (system component-path)
  (setf component-path (%coerce-component-path component-path))
  (unless (any component-path)
    (error "no component path specified"))
  (let ((component (system-component-by-path system component-path)))
    (unless component
      (error "component does not exist: '~A'" component-path))

    ;;Find the component before this one
    (when-let* ((module (system-component-by-path system (skip-last component-path 1)))
                (components (module-components module))
                (prev-component (%item-before component (to-list (vchildren components)))))
      (let ((prev-cell (member prev-component (children components)))
            (cell (member component (children components))))
        (rotatef (car prev-cell) (car cell))))))

(defun system-move-component-down (system component-path)
  (setf component-path (%coerce-component-path component-path))
  (unless (any component-path)
    (error "no component path specified"))
  (let ((component (system-component-by-path system component-path)))
    (unless component
      (error "component does not exist: '~A'" component-path))

    ;;Find the component after this one
    (when-let* ((module (system-component-by-path system (skip-last component-path 1)))
                (components (module-components module))
                (next-component (%item-after component (to-list (vchildren components)))))
      ;;swap cars
      (let ((next-cell (member next-component (children components)))
            (cell (member component (children components))))
        (rotatef (car next-cell) (car cell))))))

(defclass asd-file ()
  ((path
    :type pathname
    :initarg :path
    :initform (error "asd-file: must supply path")
    :reader asd-file-path)
   (nodes
    :type list
    :accessor asd-file-nodes)))

(defmethod initialize-instance :after ((obj asd-file) &key)
  (setf (slot-value obj 'nodes) (to-list (%parse-sexp-file (asd-file-path obj)))))

(defun read-asd-file (path)
  (make-instance 'asd-file :path path))

(defun write-asd-file (asd-file stream)
  (dolist (node (asd-file-nodes asd-file))
    (%write-node node stream)))

(defun asd-file-systems (asd-file)
  (where (asd-file-nodes asd-file) #'%system-node-p))
