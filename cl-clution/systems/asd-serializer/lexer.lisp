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

(defun %make-lexeme (type text properties)
  (list :type type :text text :properties (copy-list properties)))

(defun %lexeme-type (lexeme)
  (getf lexeme :type))

(defun %lexeme-text (lexeme)
  (getf lexeme :text))

(defun %lexeme-properties (lexeme)
  (getf lexeme :properties))

(defun %read-token (stream &optional init)
  ;;Try reading package name first
  (let* ((symbolp nil)
         (package nil)
         (external-ref nil)
         (name nil)
         (maybe-package-name
           (with-output-to-string (res)
             (let ((escaped-single nil)
                   (escaped-block nil))
               ;;Check for a pipe to start it off
               (let ((initial-char (or init (read-char stream nil nil))))
                 (unless initial-char
                   (error "eof while reading symbol name"))
                 (cond
                   ((char= initial-char #\\)
                    (setf escaped-single t)
                    (setf symbolp t))
                   ((char= initial-char #\|)
                    (setf escaped-block t)
                    (setf symbolp t))
                   (t
                    (write-char (char-upcase initial-char) res)))
                 (loop
                   :for prev-char := initial-char :then c
                   :for c := (peek-char nil stream nil nil)
                   :while c
                   :do
                      (cond
                        (escaped-single
                         (write-char c res)
                         (setf escaped-single nil))
                        ((char= c #\\)
                         (setf escaped-single t)
                         (setf symbolp t))
                        ((char= c #\|)
                         (setf escaped-block (not escaped-block))
                         (setf symbolp t))
                        (escaped-block
                         (write-char c res))
                        ((or (%terminating-p c)
                             (char= c #\:))
                         (return))
                        (t
                         (write-char (char-upcase c) res)))
                      (read-char stream)
                   :finally
                      (when (or escaped-single escaped-block)
                        (error "eof while reading escaped symbol"))))))))
    ;;See if it ended on a colon, if so, read the symbol name afterwards
    (cond
      ((eql (peek-char nil stream nil nil) #\:)
       (read-char stream)
       ;;eat a second : if it's there
       (when (eql (peek-char nil stream nil nil) #\:)
         (read-char stream)
         (setf external-ref t))
       (setf name (%read-symbol-name stream))
       (setf symbolp t)
       (setf package maybe-package-name))
      (t
       (setf name maybe-package-name)))

    (values name symbolp package external-ref)))

(defun %read-symbol-name (stream &optional init)
  ;;Check for pipe to allow escaping
  (with-output-to-string (res)
    (let ((escaped-single nil)
          (escaped-block nil))
      ;;Check for a pipe to start it off
      (let ((initial-char (or init (read-char stream nil nil))))
        (unless initial-char
          (error "eof while reading symbol"))
        (cond
          ((char= initial-char #\\)
           (setf escaped-single t))
          ((char= initial-char #\|)
           (setf escaped-block t))
          ((or (%terminating-p initial-char)
               (char= initial-char #\:))
           (error "unexpected character while reading symbol"))
          (t
           (write-char (char-upcase initial-char) res))))
      (loop :for c := (peek-char nil stream nil nil)
            :while c
            :do
               (cond
                 (escaped-single
                  (write-char c res)
                  (setf escaped-single nil))
                 ((char= c #\\)
                  (setf escaped-single t))
                 ((char= c #\|)
                  (setf escaped-block (not escaped-block)))
                 ((char= c #\:)
                  (error "unexpected package prefix while reading symbol"))
                 (escaped-block
                  (write-char c res))
                 ((%terminating-p c)
                  (return))
                 (t
                  (write-char (char-upcase c) res)))
               (read-char stream)
            :finally
               (when (or escaped-single escaped-block)
                 (error "eof while reading escaped symbol"))))))

(defun %read-string (stream)
  (with-output-to-string (res)
    (loop
      :with escaped-p := nil
      :for c := (read-char stream nil nil)
      :while c
      :do
         (cond
           (escaped-p
            (write-char c res)
            (setf escaped-p nil))
           ((char= c #\\)
            (setf escaped-p t))
           ((char= c #\")
            (return))
           (t
            (write-char c res)
            (setf escaped-p nil)))
      :finally
         (error "eof while reading string"))))

(defun %read-line-comment (stream)
  (with-output-to-string (res)
    (loop
      :for c := (read-char stream nil nil)
      :while (and c (not (char= c #\Newline)))
      :do (write-char c res))))

(defun %read-block-comment (stream)
  (with-output-to-string (res)
    (loop
      :for c := (read-char stream nil nil)
      :while c
      :do
         (cond
           ((char= c #\|)
            (let ((next-c (read-char stream nil nil)))
              (cond
                ((null next-c))
                ((char= next-c #\#)
                 (return))
                (t
                 (write-char c res)
                 (write-char next-c res)))))
           (t
            (write-char c res)))
      :finally
         (error "eof while reading block comment"))))

(defun %read-whitespace (stream &optional init)
  (with-output-to-string (res)
    (when init
      (write-char init res)
      (loop
        :for c := (peek-char nil stream nil nil)
        :while (and c (%whitespacep c))
        :do
           (write-char c res)
           (read-char stream)))))

(defun %integerp (string)
  (multiple-value-bind (value read-chars)
      (parse-integer string :junk-allowed t)
    (and value (= read-chars (length string)))))

(defun %floatp (string)
  (multiple-value-bind (value read-chars)
      (parse-float string :junk-allowed t)
    (and value (= read-chars (length string)))))

(defenumerable %lex-sexp-file (path)
  "Lex the file at `path' and return an `enumerable' sequence of lexemes"
  (loop
    :with file-string := (%slurp-file path)
    :with stream := (make-string-input-stream file-string)
    :for position := (file-position stream)
    :for c := (read-char stream nil nil)
    :while c
    :do
       (flet ((collect (data-type &rest params)
                (yield (%make-lexeme data-type (subseq file-string position (file-position stream)) params))))
         (cond
           ((char= c #\()
            (collect :lparen))
           ((char= c #\))
            (collect :rparen))
           ((char= c #\:)
            (let ((symbol-name
                    (%read-symbol-name stream)))
              (collect :symbol :keyword nil symbol-name)))
           ((char= c #\")
            (let ((string
                    (%read-string stream)))
              (collect :string string)))
           ((char= c #\;)
            (%read-line-comment stream)
            (collect :line-comment))
           ((char= c #\#)
            (let ((next-c (read-char stream nil nil)))
              (cond
                ((null next-c)
                 (error "syntax error"))
                ((char= next-c #\()
                 (collect :lvecparen))
                ((char= next-c #\+)
                 (collect :feature+))
                ((char= next-c #\-)
                 (collect :feature-))
                ((char= next-c #\|)
                 (%read-block-comment stream)
                 (collect :block-comment))
                ((char= next-c #\:)
                 (let ((symbol-name
                         (%read-symbol-name stream)))
                   (collect :symbol :external nil symbol-name)))
                ((or (char= next-c #\P)
                     (char= next-c #\p))
                 (collect :pathname))
                ((char= next-c #\.)
                 (collect :eval))
                (t
                 (error "syntax error")))))
           ((%whitespacep c)
            (%read-whitespace stream c)
            (collect :whitespace))
           (t
            (multiple-value-bind (name definitely-symbol package external-ref-p)
                (%read-token stream c)
              (if definitely-symbol
                  (collect :symbol package external-ref-p name)
                  (cond
                    ((%integerp name)
                     (collect :token))
                    ((%floatp name)
                     (collect :token))
                    ;;TODO %fractionalp
                    (t
                     (collect :symbol nil nil name))))))))))
