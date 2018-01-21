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

(defun %slurp-file (path &optional (external-format :utf-8))
  "Slurps the file at `path' into a string and returns it."
  (with-output-to-string (str)
    (with-open-file (stream path :direction :input :external-format external-format)
      (loop
        :for line := (read-line stream nil nil)
        :while line
        :do
           (write-line line str)))))

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
