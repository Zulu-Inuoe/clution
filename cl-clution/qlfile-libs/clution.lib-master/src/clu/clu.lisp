;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:clution.lib.clu)

(defun %coerce-dir-path (path)
  (etypecase path
    (null nil)
    (string-designator (list (string path)))
    (enumerable (select path #'string))))

(defun %dir-node-p (node)
  (and (%list-node-p node)
       (%symbol-node-p (sexp-list-nth node 0))
       (sexp-symbol-match (sexp-list-nth node 0) "DIR" :keyword)))

(defun dir-node-name (node)
  (sexp-list-getf node "NAME"))

(defun dir-node-items (node)
  (sexp-list-getf node "ITEMS"))

(defun dir-ensure-items (node)
  (unless (dir-node-items node)
    (%append-to-list-node node (make-instance 'sexp-symbol-node :name "ITEMS" :package :keyword))
    (%append-to-list-node node (make-instance 'sexp-list-node))))

(defun dir-by-path (dir dir-path)
  (setf dir-path (%coerce-dir-path dir-path))
  (labels ((recurse (node path)
             (cond
               ((move-next path)
                (when-let* ((items (dir-node-items node))
                            (child (->  (vchildren items)
                                        (where #'%dir-node-p)
                                        (where (lambda (child)
                                                 (string= (string-node-string (dir-node-name child))
                                                          (current path))))
                                        (efirst))))
                  (recurse child path)))
               (t
                node))))
    (recurse dir (get-enumerator dir-path))))

(defun dir-node-plist (node path)
  (let ((plist (list)))
    (push :dir plist)
    (flet ((add-prop (prop-name prop-value)
             (push prop-name plist)
             (push prop-value plist)))
      (add-prop :name (string-node-string (dir-node-name node)))
      (add-prop :items
                (when-let ((items (dir-node-items node)))
                  (-> (vchildren items)
                      (select (lambda (i) (clu-item-plist i path)))
                      (to-list)))))
    (nreverse plist)))

(defun %system-node-p (node)
  (and (%list-node-p node)
       (%symbol-node-p (sexp-list-nth node 0))
       (sexp-symbol-match (sexp-list-nth node 0) "SYSTEM" :keyword)))

(defun system-node-path (node)
  (sexp-list-getf node "PATH"))

(defun system-node-type (node)
  (sexp-list-getf node "TYPE"))

(defun system-node-plist (node path)
  (let ((plist (list)))
    (push :system plist)
    (flet ((add-prop (prop-name prop-value)
             (push prop-name plist)
             (push prop-value plist)))
      (add-prop :path
                (namestring
                 (%expand-pathname
                  (string-node-string (system-node-path node))
                  (uiop:pathname-directory-pathname path))))
      (add-prop :type (make-keyword (symbol-node-name (system-node-type node)))))
    (nreverse plist)))

(defun clu-clu-dir (clu)
  (sexp-list-getf clu "CLU-DIR"))

(defun clu-item-plist (item path)
  (cond
    ((%system-node-p item)
     (system-node-plist item path))
    ((%dir-node-p item)
     (dir-node-plist item path))
    (t
     (error "unrecognized item type: ~A" item))))

(defun clu-items (clu)
  (dir-node-items clu))

(defun clu-ensure-items (clu)
  (dir-ensure-items clu))

(defun %clu-node-p (node)
  (and (%list-node-p node)
       (%symbol-node-p (sexp-list-nth node 0))
       (sexp-symbol-match  (sexp-list-nth node 0) "CLUTION")))

(defun clu-add-dir (clu dir-path name)
  (let ((dir (dir-by-path clu dir-path)))
    (unless dir
      (error "dir does not exist at path: '~A'" dir-path))
    (dir-ensure-items dir)
    (let ((items (dir-node-items dir)))
      (when (-> (vchildren items)
                (where #'%dir-node-p)
                (any* (lambda (c)
                        (string= (string-node-string (dir-node-name c)) name))))
        (error "dir already contains dir with name '~A'" name))
      (%append-to-list-node
       items
       (make-instance
        'sexp-list-node
        :children
        (list
         (make-instance 'sexp-symbol-node :name "DIR" :package :keyword)
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-symbol-node :name "NAME" :package :keyword)
         (make-instance 'sexp-whitespace-node :text " ")
         (make-instance 'sexp-string-node :string name)
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-symbol-node :name "ITEMS" :package :keyword)
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-list-node)))))))

(defun clu-remove-dir (clu dir-path)
  (let ((dir (dir-by-path clu dir-path)))
    (unless dir
      (error "dir does not exist at path: '~A'" dir-path))
    (%delete-from-list-node (sexp-node-parent dir) dir)))

(defun clu-add-system (clu dir-path system-path system-type)
  (let ((dir (dir-by-path clu dir-path)))
    (unless dir
      (error "dir does not exist at path: '~A'" dir-path))
    (dir-ensure-items dir)
    (let ((items (dir-node-items dir)))
      (when (-> (vchildren items)
                (where #'%system-node-p)
                (any* (lambda (sys-node)
                        (%pathname-equal (string-node-string (system-node-path sys-node))
                                         system-path))))
        (error "clu already contains system '~A'" system-path))
      (%append-to-list-node
       items
       (make-instance
        'sexp-list-node
        :children
        (list
         (make-instance 'sexp-symbol-node :name "SYSTEM" :package :keyword)
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-symbol-node :name "PATH" :package :keyword)
         (make-instance 'sexp-whitespace-node :text " ")
         (make-instance 'sexp-string-node :string (namestring system-path))
         (%make-indent-node (+ (%node-indention-level items) 2) nil)
         (make-instance 'sexp-symbol-node :name "TYPE" :package :keyword)
         (make-instance 'sexp-whitespace-node :text " ")
         (make-instance 'sexp-symbol-node :name (symbol-name system-type) :package :keyword)))))))

(defun clu-remove-system (clu dir-path system-path)
  (let ((dir (dir-by-path clu dir-path)))
    (unless dir
      (error "dir does not exist at path: '~A'" dir-path))
    (let ((items (dir-node-items dir)))
      (unless items
        (error "no such system '~A' at path '~A'" system-path dir-path))
      (let ((system (-> (vchildren items)
                        (where #'%system-node-p)
                        (where (lambda (sys-node)
                                  (%pathname-equal (string-node-string (system-node-path sys-node))
                                                   system-path)))
                        (efirst))))
        (unless system
          (error "no such system '~A' at path '~A'" system-path dir-path))
        (%delete-from-list-node items system)))))

(defun clu-plist (clu path)
  ;;TODO Refactor to use the proper clu objects rather than repeating code
  (let* ((plist (list))
         (dir (uiop:pathname-directory-pathname path))
         (clu-dir
           (%pathname-as-directory
            (%expand-pathname
             (if-let ((clu-dir (clu-clu-dir clu)))
               (string-node-string clu-dir)
               ".clu/")
             dir)))
         (qlfile-libs-dir
           (%pathname-as-directory
            (%expand-pathname
             "qlfile-libs"
             clu-dir))))
    (push :clution plist)
    (flet ((add-prop (prop-name prop-value)
             (push prop-name plist)
             (push prop-value plist)))
      (add-prop :path (namestring path))
      (add-prop :items
                (when-let ((items (clu-items clu)))
                  (-> (vchildren items)
                      (select (lambda (i) (clu-item-plist i path)))
                      (to-list))))
      (add-prop :clu-dir (namestring clu-dir))
      (add-prop :qlfile-libs-dir (namestring qlfile-libs-dir)))
    (nreverse plist)))

(defclass clu-file-item ()
  ((clu-file
    :type clu-file
    :initarg :clu-file
    :initform (error "clu-file-item: must supply clu")
    :reader clu-file-item-clu-file)
   (node
    :type sexp-list-node
    :initarg :node
    :initform (error "clu-file-item: must supply node")
    :reader clu-file-item-node)
   (parent
    :type clu-file-item
    :initarg :parent
    :initform nil
    :reader clu-file-item-parent)))

(defclass clu-file-dir-item (clu-file-item)
  ())

(defun clu-file-dir-item-items (clu-file-dir-item
                                &aux
                                  (clu (clu-file-item-clu-file clu-file-dir-item)))
  (when-let (items-node (dir-node-items (clu-file-item-node clu-file-dir-item)))
    (-> (vchildren items-node)
        (select (lambda (node)
                  (cond
                    ((%dir-node-p node)
                     (make-instance 'clu-file-dir-item :clu clu :node node :parent clu-file-dir-item))
                    ((%system-node-p node)
                     (make-instance 'clu-file-system-item :clu clu :node node :parent clu-file-dir-item))
                    (t
                     (error "malformed clu item: '~A'" node))))))))

(defun clu-file-dir-item-systems (clu-file-dir-item)
  (-> (clu-file-dir-item-items clu-file-dir-item)
      (select-many (lambda (item)
                     (etypecase item
                       (clu-file-dir-item
                        (clu-file-dir-item-systems item))
                       (clu-file-system-item
                        (list item)))))))

(defclass clu-file-system-item (clu-file-item)
  ())

(defun clu-file-system-item-path (clu-file-system-item)
  (%expand-pathname
   (string-node-string (system-node-path (clu-file-item-node clu-file-system-item)))
   (clu-file-dir (clu-file-item-clu-file clu-file-system-item))))

(defclass clu-file ()
  ((path
    :type pathname
    :initarg :path
    :initform (error "clu-file: must supply path")
    :reader clu-file-path)
   (eol-style
    :type %eol-style
    :reader clu-file-eol-style)
   (nodes
    :type list
    :accessor clu-file-nodes)))

(defmethod initialize-instance :after ((obj clu-file) &key (eol-style *%eol-style* eol-style-sup-p))
  (check-type eol-style %eol-style)
  (cond
    ((probe-file (clu-file-path obj))
     (setf (slot-value obj 'nodes) (to-list (%parse-sexp-file (clu-file-path obj)))
           (slot-value obj 'eol-style)
           (if eol-style-sup-p
               eol-style
               (%guess-eol-style (clu-file-path obj) eol-style))))
    (t
     (setf (slot-value obj 'nodes) nil
           (slot-value obj 'eol-style) eol-style))))

(defun clu-file-dir (clu-file)
  (uiop:pathname-directory-pathname (clu-file-path clu-file)))

(defun clu-file-clu-dir (clu-file)
  (%pathname-as-directory
   (%expand-pathname
    (if-let (clu-dir-node
             (when-let (clu (clu-file-clu clu-file))
               (clu-clu-dir clu)))
      (string-node-string clu-dir-node)
      ".clu")
    (clu-file-dir clu-file))))

(defun clu-file-qlfile-fetch-dir (clu-file)
  (%pathname-as-directory
   (%expand-pathname
    "qlfile-fetch"
    (clu-file-clu-dir clu-file))))

(defun clu-file-qlfile-libs-dir (clu-file)
  (%pathname-as-directory
   (%expand-pathname
    "qlfile-libs"
    (clu-file-clu-dir clu-file))))

(defun clu-file-items (clu-file)
  (when-let* ((clu-node (clu-file-clu clu-file))
              (items-node (clu-items clu-node)))
    (-> (vchildren items-node)
        (select (lambda (node)
                  (cond
                    ((%dir-node-p node)
                     (make-instance 'clu-file-dir-item :clu-file clu-file :node node))
                    ((%system-node-p node)
                     (make-instance 'clu-file-system-item :clu-file clu-file :node node))
                    (t
                     (error "malformed clu item: '~A'" node))))))))

(defun clu-file-systems (clu-file)
  (-> (clu-file-items clu-file)
      (select-many (lambda (item)
                     (etypecase item
                       (clu-file-dir-item
                        (clu-file-dir-item-systems item))
                       (clu-file-system-item
                        (list item)))))))

(defun read-clu-file (path)
  (make-instance 'clu-file :path (pathname path)))

(defun write-clu-file (clu-file stream)
  (dolist (node (clu-file-nodes clu-file))
    (%write-node node stream)))

(defun clu-file-clu (clu-file)
  (efirst* (clu-file-nodes clu-file) #'%clu-node-p))

(defun clu-file-ensure-clu (clu-file
                            &aux
                              (*%eol-style* (clu-file-eol-style clu-file)))
  (unless (clu-file-clu clu-file)
    (when (and (any (clu-file-nodes clu-file))
               (not (%opaque-node-ends-with-newline (elast (clu-file-nodes clu-file)))))
      (setf (clu-file-nodes clu-file)
            (nconc (clu-file-nodes clu-file)
                   (list
                    (%make-indent-node 0 nil)))))
    (setf (clu-file-nodes clu-file)
          (nconc (clu-file-nodes clu-file)
                 (list
                  (make-instance
                   'sexp-list-node
                   :children
                   (list (make-instance 'sexp-symbol-node :name "CLUTION" :package :keyword)))
                  (%make-indent-node 0 nil))))))

(defun clu-file-add-dir (clu-file dir-path name)
  (clu-file-ensure-clu clu-file)
  (let* ((*%eol-style* (clu-file-eol-style clu-file))
         (clu (clu-file-clu clu-file)))
    (clu-add-dir clu dir-path name)))

(defun clu-file-remove-dir (clu-file dir-path)
  (let ((*%eol-style* (clu-file-eol-style clu-file))
        (clu (clu-file-clu clu-file)))
    (clu-remove-dir clu dir-path)))

(defun clu-file-add-system (clu-file dir-path system-path type)
  (clu-file-ensure-clu clu-file)
  (let ((*%eol-style* (clu-file-eol-style clu-file))
        (clu (clu-file-clu clu-file))
        (rel-path (%relative-pathname system-path (clu-file-dir clu-file))))
    (clu-add-system clu dir-path rel-path type)))

(defun clu-file-remove-system (clu-file dir-path system-path)
  (let ((*%eol-style* (clu-file-eol-style clu-file))
        (clu (clu-file-clu clu-file))
        (rel-path (%relative-pathname system-path (clu-file-dir clu-file))))
    (clu-remove-system clu dir-path rel-path)))

(defun clu-file-plist (clu-file)
  (when-let ((clu (clu-file-clu clu-file)))
    (clu-plist clu (clu-file-path clu-file))))
