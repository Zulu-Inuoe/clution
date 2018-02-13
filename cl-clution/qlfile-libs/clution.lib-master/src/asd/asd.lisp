;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:clution.lib.asd)

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
     (string-downcase (symbol-node-name name)))
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

(defun %coerce-path-string-file (path)
  (etypecase path
    (sexp-symbol-node
     (make-pathname :name (string-downcase (symbol-node-name path)) :type "lisp"))
    (sexp-string-node
     (make-pathname :name (string-node-string path) :type "lisp"))
    ;;TODO need pathname handling
    ))

(defun %coerce-path-string-module (path)
  (etypecase path
    (sexp-symbol-node
     (make-pathname :directory (list :relative (string-downcase (symbol-node-name path)))))
    (sexp-string-node
     (make-pathname :directory (list :relative (string-node-string path))))
    ;;TODO need pathname handling
    ))

(defun %coerce-path-string-static-file (path)
  (etypecase path
    (sexp-symbol-node
     (parse-namestring (string-downcase (symbol-node-name path))))
    (sexp-string-node
     (parse-namestring (string-node-string path)))
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

(defun %static-file-node-p (component)
  (and (%list-node-p component)
       (%symbol-node-p (sexp-list-nth component 0))
       (sexp-symbol-match (sexp-list-nth component 0) "STATIC-FILE" :keyword)))

(defun %system-node-p (node)
  (and (%list-node-p node)
       (when-let ((first-child (efirst (vchildren node))))
         (and (%symbol-node-p first-child)
              (sexp-symbol-match first-child "DEFSYSTEM")))))

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

(defun %create-static-file-component (name &key parent pathname)
  (let ((nodes ()))
    (push (make-instance 'sexp-symbol-node :name "STATIC-FILE" :package :keyword) nodes)
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

(defun component-name (component)
  (let ((name-prop
          (or
           (sexp-list-getf component "FILE")
           (sexp-list-getf component "MODULE")
           (sexp-list-getf component "STATIC-FILE"))))
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
  (%delete-from-list-node (sexp-node-parent component) component))

(defun module-name (module)
  (component-name module))

(defun module-components (module)
  (sexp-list-getf module "COMPONENTS"))

(defun module-serial (module)
  (sexp-list-getf module "SERIAL"))

(defun module-ensure-components (module)
  (unless (module-components module)
    (let ((indent-offset (if (%system-node-p module) 2 1)))
      (%append-to-list-node module (make-instance 'sexp-symbol-node :name "COMPONENTS" :package :keyword) indent-offset)
      (%append-to-list-node module (make-instance 'sexp-list-node :children ()) indent-offset)))
  (values))

(defun module-component-by-path (module component-path)
  (labels ((recurse (node path)
             (cond
               ((move-next path)
                (when-let* ((components (module-components node))
                            (child (efirst* (vchildren components)
                                            (lambda (child)
                                              (string= (%coerce-name-string (component-name child))
                                                       (current path))))))
                  (recurse child path)))
               (t
                node))))
    (recurse module (get-enumerator component-path))))

(defun module-ensure-depends-on (module)
  (unless (component-depends-on module)
    (let ((indent-offset (if (%system-node-p module) 2 1)))
      (%append-to-list-node module (make-instance 'sexp-symbol-node :name "DEPENDS-ON" :package :keyword) indent-offset)
      (%append-to-list-node module (make-instance 'sexp-list-node :children ()) indent-offset)))
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
           (%expand-pathname
            (cond
              ((%file-node-p component)
               (%coerce-path-string-file
                 (or
                  (component-pathname component)
                  (component-name component))))
              ((%module-node-p component)
               (%coerce-path-string-module
                 (or
                  (component-pathname component)
                  (component-name component))))
              ((%static-file-node-p component)
               (%coerce-path-string-static-file
                 (or
                  (component-pathname component)
                  (component-name component)))))
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
        ((%static-file-node-p component)
         (add-prop :type :static-file))
        (t
         (error "malformed component '~A'" component))))
    (nreverse plist)))

(defun system-plist (system path)
  (let* ((plist (list))
         (dir (uiop:pathname-directory-pathname path))
         (pathname
           (or
            (when-let ((pathname-node (system-pathname system)))
              (%expand-pathname
               (%coerce-path-string-file pathname-node)
               dir))
            dir)))
    (flet ((add-prop (name value)
             (push name plist)
             (push value plist)))
      (add-prop :name (%coerce-name-string (system-name system)))
      (add-prop :pathname (namestring pathname))
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
  (module-component-by-path system component-path))

(defun system-rename-component (system component-path new-name)
  (let ((component (system-component-by-path system component-path))
        (new-component (system-component-by-path system (-> component-path
                                                            (skip-last 1)
                                                            (eappend new-name)))))
    (unless component
      (error "component does not exist: '~A'" component-path))
    (when new-component
      (error "cannot rename to '~A': component already exists with that name" new-name))
    (when (component-pathname component)
      (error "cannot yet handle components with pathnames"))

    (let ((name-prop (component-name component)))
      (setf (string-node-string name-prop) new-name))))

(defun system-add-file-component (system module-path name &key pathname)
  (setf module-path (%coerce-component-path module-path))
  (let ((module (system-component-by-path system  module-path)))
    (unless module
      (error "module does not exist: '~A'" module-path))
    (when (module-component-by-path module (%coerce-component-path name))
      (error "component already exists in module: ~A" name))
    (module-ensure-components module)

    (let ((components (module-components module)))
      (%append-to-list-node components (%create-file-component name :pathname pathname)))))

(defun system-add-module-component (system module-path name &key pathname)
  (setf module-path (%coerce-component-path module-path))
  (let ((module (system-component-by-path system  module-path)))
    (unless module
      (error "module does not exist: '~A'" module-path))
    (when (module-component-by-path module (%coerce-component-path name))
      (error "component already exists in module: ~A" name))
    (module-ensure-components module)

    (let* ((components (module-components module)))
      (%append-to-list-node
       components
       (%create-module-component name :parent components :pathname pathname)))))

(defun system-add-static-file-component (system module-path name &key pathname)
  (setf module-path (%coerce-component-path module-path))
  (let ((module (system-component-by-path system  module-path)))
    (unless module
      (error "module does not exist: '~A'" module-path))
    (when (module-component-by-path module (%coerce-component-path name))
      (error "component already exists in module: ~A" name))
    (module-ensure-components module)

    (let ((components (module-components module)))
      (%append-to-list-node components (%create-static-file-component name :pathname pathname)))))

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
    (let ((depends-on (component-depends-on module)))
      (unless depends-on
        (error "no such dependency '~A'" name))

      (let* ((node (efirst* (vchildren depends-on)
                            (lambda (node)
                              (string= (%coerce-name-string node) name)))))
        (unless node
          (error "no such dependency: '~A'" name))
        (%delete-from-list-node depends-on node)))))

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

(defclass asd-file-system-component ()
  ((asd-file-system
    :type asd-file-system
    :initarg :asd-file-system
    :initform (error "asd-file-system-component: must supply asd-file-system")
    :reader asd-file-system-component-system)
   (node
    :type sexp-list-node
    :initarg :node
    :initform (error "asd-file-system-component: must supply node")
    :reader asd-file-system-component-node)
   (parent
    :type (or null asd-file-system-component)
    :initarg :parent
    :initform nil
    :reader asd-file-system-component-parent)))

(defun asd-file-system-component-name (component)
  (%coerce-name-string (component-name (asd-file-system-component-node component))))

(defgeneric asd-file-system-component-path (component)
  (:documentation "Calculates a pathname to `component'."))

(defclass asd-file-system-file-component (asd-file-system-component)
  ())

(defmethod asd-file-system-component-path ((component asd-file-system-file-component))
  (let ((parent-dir (if-let ((parent (asd-file-system-component-parent component)))
                      (asd-file-system-component-path component)
                      (asd-file-system-dir (asd-file-system-component-system component)))))
    (%expand-pathname
     (%coerce-path-string-file
      (or (component-pathname (asd-file-system-component-node component))
          (component-name (asd-file-system-component-node component))))
     parent-dir)))

(defclass asd-file-system-module-component (asd-file-system-component)
  ())

(defmethod asd-file-system-component-path ((component asd-file-system-module-component))
  (let ((parent-dir (if-let ((parent (asd-file-system-component-parent component)))
                      (asd-file-system-component-path component)
                      (asd-file-system-dir (asd-file-system-component-system component)))))
    (%pathname-as-directory
     (%expand-pathname
      (%coerce-path-string-module
       (or (component-pathname (asd-file-system-component-node component))
           (component-name (asd-file-system-component-node component))))
      parent-dir))))

(defun asd-file-system-module-component-components (module
                                                    &aux
                                                      (asd-file-system (asd-file-system-component-system module)))
  (when-let (module-components (module-components (asd-file-system-component-node module)))
    (-> (vchildren module-components)
        (select (lambda (node)
                  (cond
                    ((%file-node-p node)
                     (make-instance 'asd-file-system-file-component
                                    :asd-file-system asd-file-system
                                    :node node
                                    :parent module))
                    ((%module-node-p node)
                     (make-instance 'asd-file-system-module-component
                                    :asd-file-system asd-file-system
                                    :node node
                                    :parent module))
                    ((%static-file-node-p node)
                     (make-instance 'asd-file-system-static-file-component
                                    :asd-file-system asd-file-system
                                    :node node
                                    :parent module))
                    (t
                     (error "asd-file-system: unrecognized component node: '~A'" node))))))))

(defclass asd-file-system-static-file-component (asd-file-system-component)
  ())

(defmethod asd-file-system-component-path ((component asd-file-system-static-file-component))
  (let ((parent-dir (if-let ((parent (asd-file-system-component-parent component)))
                      (asd-file-system-component-path component)
                      (asd-file-system-dir (asd-file-system-component-system component)))))
    (%expand-pathname
     (%coerce-path-string-static-file
      (or (component-pathname (asd-file-system-component-node component))
           (component-name (asd-file-system-component-node component))))
     parent-dir)))

(defclass asd-file-system ()
  ((asd-file
    :type asd-file
    :initarg :asd-file
    :initform (error "asd-file-system: must supply asd-file")
    :reader asd-file-system-file)
   (node
    :type sexp-list-node
    :initarg :node
    :initform (error "asd-file-system: must supply node")
    :reader asd-file-system-node)))

(defun asd-file-system-name (asd-file-system)
  "Name of the system"
  (-> (asd-file-system-node asd-file-system)
      (sexp-list-nth 1)
      (%coerce-name-string)))

(defun asd-file-system-dir (asd-file-system
                            &aux
                              (base-dir (uiop:pathname-directory-pathname
                                         (asd-file-path (asd-file-system-file asd-file-system)))))
  "Base directory for system components."
  (%pathname-as-directory
   (or
    (when-let ((pathname-node (system-pathname (asd-file-system-node asd-file-system))))
      (%expand-pathname
       (%coerce-path-string-file pathname-node)
       base-dir))
    base-dir)))

(defun asd-file-system-components (asd-file-system)
  (when-let (system-components (system-components (asd-file-system-node asd-file-system)))
    (-> (vchildren system-components)
        (select (lambda (node)
                  (cond
                    ((%file-node-p node)
                     (make-instance 'asd-file-system-file-component
                                    :asd-file-system asd-file-system
                                    :node node))
                    ((%module-node-p node)
                     (make-instance 'asd-file-system-module-component
                                    :asd-file-system asd-file-system
                                    :node node))
                    ((%static-file-node-p node)
                     (make-instance 'asd-file-system-static-file-component
                                    :asd-file-system asd-file-system
                                    :node node))
                    (t
                     (error "asd-file-system: unrecognized component node: '~A'" node))))))))

(defclass asd-file ()
  ((path
    :type pathname
    :initarg :path
    :initform (error "asd-file: must supply path")
    :reader asd-file-path)
   (eol-style
    :type %eol-style
    :reader asd-file-eol-style)
   (nodes
    :type list
    :accessor asd-file-nodes)))

(defun asd-file-dir (asd-file)
  (uiop:pathname-directory-pathname (asd-file-path asd-file)))

(defmethod initialize-instance :after ((obj asd-file) &key (eol-style *%eol-style* eol-style-sup-p))
  (check-type eol-style %eol-style)
  (cond
    ((probe-file (asd-file-path obj))
     (setf (slot-value obj 'nodes) (to-list (%parse-sexp-file (asd-file-path obj)))
           (slot-value obj 'eol-style)
           (if eol-style-sup-p
               eol-style
               (%guess-eol-style (asd-file-path obj) *%eol-style*))))
    (t
     (setf (slot-value obj 'nodes) nil
           (slot-value obj 'eol-style) eol-style))))

(defun read-asd-file (path)
  (make-instance 'asd-file :path path))

(defun write-asd-file (asd-file stream)
  (dolist (node (asd-file-nodes asd-file))
    (%write-node node stream)))

(defun asd-file-systems (asd-file)
  (-> (asd-file-system-nodes asd-file)
      (select (lambda (node)
                (make-instance 'asd-file-system :asd-file asd-file :node node)))))

(defun asd-file-system-nodes (asd-file)
  (-> (asd-file-nodes asd-file)
      (where #'%system-node-p)))

(defun asd-file-system-plists (asd-file)
  (-> (asd-file-system-nodes asd-file)
      (select (lambda (system) (system-plist system (asd-file-path asd-file))))
      (to-list)))

(defun asd-file-add-file-component (asd-file component-path component-name
                                    &aux
                                      (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file))
          (rel-name (namestring (%relative-pathname component-name (asd-file-dir asd-file)))))
      (system-add-file-component system (cdr component-path) rel-name))))

(defun asd-file-add-module-component (asd-file component-path component-name
                                      &aux
                                        (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file))
          (rel-name (namestring (%directory-pathname
                                 (%relative-pathname component-name (asd-file-dir asd-file))))))
      (system-add-module-component system (cdr component-path) rel-name))))

(defun asd-file-add-static-file-component (asd-file component-path component-name
                                           &aux
                                             (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file))
          (rel-name (namestring (%relative-pathname component-name (asd-file-dir asd-file)))))
      (system-add-static-file-component system (cdr component-path) rel-name))))

(defun asd-file-rename-component (asd-file component-path new-name
                                  &aux
                                    (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file)))
      (system-rename-component system (rest component-path) new-name))))

(defun asd-file-move-component-up (asd-file component-path
                                   &aux
                                     (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file)))
      (system-move-component-up system (rest component-path)))))

(defun asd-file-move-component-down (asd-file component-path
                                     &aux
                                       (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file)))
      (system-move-component-down system (rest component-path)))))

(defun asd-file-remove-component (asd-file component-path
                                  &aux
                                    (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file)))
      (system-remove-component system (rest component-path)))))

(defun asd-file-add-depends-on (asd-file component-path dependency-name
                                &aux
                                  (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file)))
      (system-add-depends-on system (rest component-path) dependency-name))))

(defun asd-file-remove-depends-on (asd-file component-path dependency-name
                                   &aux
                                     (system-name (first component-path)))
  (let ((system (efirst* (asd-file-system-nodes asd-file)
                         (lambda (system)
                           (string= (%coerce-name-string (system-name system)) system-name)))))
    (unless system
      (error "system does not exist: '~A'" system-name))

    (let ((*%eol-style* (asd-file-eol-style asd-file)))
      (system-remove-depends-on system (rest component-path) dependency-name))))
