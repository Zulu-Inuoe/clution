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

(defmethod %write-node ((node sexp-opaque-node) stream)
  (format stream "~A" (opaque-node-text node)))

(defclass sexp-whitespace-node (sexp-opaque-node)
  ())

(defclass sexp-parent-node (sexp-node)
  ((children
    :initform nil
    :type list
    :accessor children)))

(defun vchildren (parent-node)
  (where (children parent-node) (lambda (n) (not (typep n 'sexp-opaque-node)))))

(defmethod initialize-instance :after ((obj sexp-parent-node) &key children)
  (setf (slot-value obj 'children) (to-list children))
  (do-enumerable (c (children obj))
    (setf (sexp-node-parent c) obj)))

(defclass sexp-list-node (sexp-parent-node)
  ())

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
  (efirst (skip (skip-until (vchildren sexp-list)
                            (lambda (node)
                              (and (typep node 'sexp-symbol-node)
                                   (sexp-symbol-match node prop-name prop-package))))
                1)))

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
      ((:line-comment :block-comment)
       (%read-sexp-opaque enumerator)))))

(defenumerable %parse-sexp-file (path)
  "Parses the sexp's in `path' and returns an `enumerable' of each node within it."
  (loop
    :with enumerator := (get-enumerator (%lex-sexp-file path))
    :for node := (%read-sexp-node enumerator)
    :while node
    :do (yield node)))
