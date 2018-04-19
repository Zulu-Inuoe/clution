;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:clution.lib.cl-sexp)

(defgeneric %write-node (node stream))

(defgeneric %node-string (node)
  (:method (node)
    (with-output-to-string (s)
      (%write-node node s))))

(defclass sexp-node ()
  ((parent
    :initarg :parent
    :initform nil
    :type sexp-parent-node
    :reader sexp-node-parent)))

(defgeneric (setf sexp-node-parent) (value node))

(defmethod (setf sexp-node-parent) (value (node sexp-node))
  (check-type value sexp-parent-node)
  (setf (slot-value node 'parent) value))

(defclass sexp-opaque-node (sexp-node)
  ((text
    :initarg :text
    :initform (error "sexp-opaque-node: must supply text")
    :reader opaque-node-text)))

(defmethod print-object ((object sexp-opaque-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (opaque-node-text object))))

(defun %opaque-node-p (node)
  (typep node 'sexp-opaque-node))

(defmethod %write-node ((node sexp-opaque-node) stream)
  (format stream "~A" (opaque-node-text node)))

(defclass sexp-whitespace-node (sexp-opaque-node)
  ())

(defun %whitespace-node-p (node)
  (typep node 'sexp-whitespace-node))

(defclass sexp-comment-node (sexp-opaque-node)
  ())

(defun %comment-node-p (node)
  (typep node 'sexp-comment-node))

(defclass sexp-feature-node (sexp-node)
  ((feature-type
    :type (member :feature+ :feature-)
    :accessor feature-type)
   (whitespace-and-comments
    :type list
    :accessor whitespace-and-comments)
   (feature-expr-node
    :type sexp-node
    :accessor feature-expr-node)))

(defmethod print-object ((obj sexp-feature-node) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~S: ~S" (feature-type obj) (feature-expr-node obj))))

(defmethod %write-node ((node sexp-feature-node) stream)
  (ecase (feature-type node)
    (:feature+
     (format stream "#+"))
    (:feature-
     (format stream "#-")))
  (do-enumerable (n (whitespace-and-comments node))
    (%write-node n stream))
  (%write-node (feature-expr-node node) stream))

(defmethod initialize-instance :after ((obj sexp-feature-node)
                                       &key
                                         feature-type
                                         whitespace-and-comments
                                         feature-expr)
  (check-type feature-type (member :feature+ :feature-))
  (check-type whitespace-and-comments enumerable)
  (check-type feature-expr sexp-node)

  (setf (slot-value obj 'feature-type) feature-type
        (slot-value obj 'whitespace-and-comments) (to-list whitespace-and-comments)
        (slot-value obj 'feature-expr-node) feature-expr))

(defun %feature-node-p (node)
  (typep node 'sexp-feature-node))

(defclass sexp-parent-node (sexp-node)
  ((children
    :initform nil
    :type list
    :accessor children)))

(defun %parent-node-p (node)
  (typep node 'sexp-parent-node))

(defun vchildren (parent-node)
  (where (children parent-node)
         (lambda (n)
           (and (not (%opaque-node-p n))
                (not (%feature-node-p n))))))

(defmethod initialize-instance :after ((obj sexp-parent-node) &key children)
  (setf (slot-value obj 'children) (to-list children))
  (do-enumerable (c (children obj))
    (setf (sexp-node-parent c) obj)))

(defclass sexp-list-node (sexp-parent-node)
  ())

(defun %list-node-p (node)
  (typep node 'sexp-list-node))

(defmethod print-object ((object sexp-list-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(")
    (let ((first t))
      (do-enumerable (c (take (vchildren object) 2))
        (if first
            (setf first nil)
            (format stream " "))
        (format stream "~A" c)))
    (format stream ")")))

(defmethod %write-node ((node sexp-list-node) stream)
  (format stream "(")
  (dolist (c (children node))
    (%write-node c stream))
  (format stream ")"))

(defun sexp-list-nth (sexp-list n)
  (efirst (skip (vchildren sexp-list) n)))

(defun sexp-list-getf (sexp-list prop-name &optional (prop-package :keyword))
  (-> (vchildren sexp-list)
      (skip-until (lambda (node)
                    (and (typep node 'sexp-symbol-node)
                         (sexp-symbol-match node prop-name prop-package))))
      (skip 1)
      (efirst)))

(defun %append-to-list-node (list-node new-node &optional (indent-offset 1)
                             &aux
                               (indent (+ (%node-indention-level list-node)
                                          indent-offset)))
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

(defun %delete-from-list-node (list-node node)
  (let* ((cell (member node (children list-node))))
    ;;Delete any white space that follows it
    (loop :while (and (cdr cell) (%whitespace-node-p (cadr cell)))
          :do (setf (cdr cell) (cddr cell)))

    ;;If it's the last node in the parent, remove whitespace before it, too
    (when (null (cdr cell))
      (loop
        :for prev-cell := (%cell-before node (children list-node))
        :while (and prev-cell (%whitespace-node-p (car prev-cell)))
        :do (setf (children list-node) (delete (car prev-cell) (children list-node)))))

    ;;Delete it
    (setf (children list-node) (delete node (children list-node)))

    ;;If there is only whitespace left, then clear out all children
    (when (all (children list-node) #'%whitespace-node-p)
      (setf (children list-node) nil))))

(defclass sexp-vector-node (sexp-parent-node)
  ())

(defmethod %write-node ((node sexp-vector-node) stream)
  (format stream "#(")
  (dolist (c (children node))
    (%write-node c stream))
  (format stream ")"))

(defclass sexp-token-node (sexp-node)
  ())

(defgeneric token-text (token))

(defmethod print-object ((object sexp-token-node) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (token-text object))))

(defmethod %write-node ((node sexp-token-node) stream)
  (format stream "~A" (token-text node)))

(defclass sexp-symbol-node (sexp-token-node)
  ((name
    :reader symbol-node-name)
   (package
    :reader symbol-node-package)
   (external-ref-p
    :reader symbol-node-external-ref-p)))

(defun %symbol-node-p (node)
  (typep node 'sexp-symbol-node))

(defmethod initialize-instance ((obj sexp-symbol-node)
                                &key
                                  (name (error "must supply name"))
                                  package
                                  external-ref-p)
  (setf (slot-value obj 'name)
        (typecase name
          (string
           name)
          (symbol
           (symbol-name name))))
  (setf (slot-value obj 'package)
        (typecase package
          (package
           (if (eq package (find-package :keyword))
               :keyword
               (package-name package)))
          (null
           nil)
          ((or (eql :keyword) (eql :external))
           package)
          (symbol
           (cond
             ((string= (symbol-name package) "KEYWORD")
              :keyword)
             ((string= (symbol-name package) "EXTERNAL")
              :external)
             (t
              (symbol-name package))))))
  (setf (slot-value obj 'external-ref-p) (and external-ref-p t)))

(defmethod token-text ((token sexp-symbol-node))
  (with-output-to-string (res)
    (case (symbol-node-package token)
      (:keyword
       (format res ":"))
      (:external
       (format res "#:"))
      ((nil))
      (t
       (let ((*print-gensym* nil)
             (*print-case* :downcase))
         (format res "~S:" (make-symbol (symbol-node-package token))))
       (when (symbol-node-external-ref-p token)
         (format res ":"))))
    (let ((*print-gensym* nil)
          (*print-case* :downcase))
      (format res "~S" (make-symbol (symbol-node-name token))))))

(defun sexp-symbol-match (sexp-symbol name &optional package)
  (and (or (not package) (equal (symbol-node-package sexp-symbol) package))
       (string= (symbol-node-name sexp-symbol) name)))

(defclass sexp-string-node (sexp-token-node)
  ((string
    :initarg :string
    :initform (error "Must supply string")
    :reader string-node-string)))

(defun %string-node-p (node)
  (typep node 'sexp-string-node))

(defmethod token-text ((token sexp-string-node))
  (format nil "~S" (string-node-string token)))

(defun (setf string-node-string) (value node)
  (setf (slot-value node 'string) (string value)))

(defun %read-sexp-list (enumerator)
  (loop
    :while (move-next enumerator)
    :for x := (current enumerator)
    :if (eq (%lexeme-type x) :rparen)
      :return (make-instance 'sexp-list-node :children children)
    :else
      :collect (%read-sexp-node enumerator t) :into children
    :finally
    (error "eof while reading list")))

(defun %read-sexp-vector (enumerator)
  (loop
    :while (move-next enumerator)
    :for x := (current enumerator)
    :if (eq (%lexeme-type x) :rparen)
      :return (make-instance 'sexp-list-node :children children)
    :else
      :collect (multiple-value-bind (node validp)
                   (%read-sexp-node enumerator t)
                 (if (not validp)
                     (error "eof while reading list child")
                     node))
        :into children
    :finally
       (error "eof while reading list")))

(defun %read-sexp-feature (enumerator)
  (let ((feature-node (current enumerator)))
    ;;Eat up nodes until we get a non-whitespace, non-comment one
    (loop
      :for node := (%read-sexp-node enumerator)
      :unless node
        :do (error "eof while reading feature")
      :if (or (%whitespace-node-p node)
              (%comment-node-p node))
        :collect node :into children
      :else
        :return (make-instance
                 'sexp-feature-node
                 :feature-type (%lexeme-type feature-node)
                 :whitespace-and-comments children
                 :feature-expr node))))

(defun %read-sexp-symbol (enumerator)
  (destructuring-bind (package external-ref-p name)
      (%lexeme-properties (current enumerator))
    (make-instance 'sexp-symbol-node
                   :package package
                   :external-ref-p external-ref-p
                   :name name)))

(defun %read-sexp-string (enumerator)
  (make-instance 'sexp-string-node :string (first (%lexeme-properties (current enumerator)))))

(defun %read-sexp-opaque (enumerator)
  (make-instance 'sexp-opaque-node :text (%lexeme-text (current enumerator))))

(defun %read-sexp-whitespace (enumerator)
  (make-instance 'sexp-whitespace-node :text (%lexeme-text (current enumerator))))

(defun %read-sexp-comment (enumerator)
  (make-instance 'sexp-comment-node :text (%lexeme-text (current enumerator))))

(defun %read-sexp-node (enumerator &optional dont-move-p)
  (unless (or dont-move-p (move-next enumerator))
    (return-from %read-sexp-node))
  (let ((lexeme (current enumerator)))
    (ecase (%lexeme-type lexeme)
      (:lparen
       (%read-sexp-list enumerator))
      (:lvecparen
       (%read-sexp-vector enumerator))
      (:rparen
       (error "unbalanced rparen"))
      (:symbol
       (%read-sexp-symbol enumerator))
      (:string
       (%read-sexp-string enumerator))
      (:token
       (%read-sexp-opaque enumerator))
      (:whitespace
       (%read-sexp-whitespace enumerator))
      ((:feature+ :feature-)
       (%read-sexp-feature enumerator))
      ((:line-comment :block-comment)
       (%read-sexp-comment enumerator)))))

(defenumerable %parse-sexp-file (path)
  "Parses the sexp's in `path' and returns an `enumerable' of each node within it."
  (loop
    :with enumerator := (get-enumerator (%lex-sexp-file path))
    :for node := (%read-sexp-node enumerator)
    :while node
    :do (yield node)))

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
       :sum (-> (opaque-node-text prev-node)
                (reverse)
                (take-while  (lambda (c) (char= c #\Space)))
                (ecount))))))

(defun %make-indent-node (indent skip-newline-p &optional parent)
  (let ((prefix-whitespace
          (with-output-to-string (res)
            (unless skip-newline-p
              (do-enumerable (c (%eol-sequence))
                (format res "~C" c)))
            (format res "~A" (make-string indent :initial-element #\Space)))))
    (make-instance 'sexp-whitespace-node
                   :parent parent
                   :text prefix-whitespace)))

(defun %opaque-node-ends-with-newline (node)
  (and (%opaque-node-p node)
       (-> *%eol-sequences*
           (select #'cdr)
           (any* (lambda (eol-seq) (ends-with-subseq eol-seq (opaque-node-text node)))))))
