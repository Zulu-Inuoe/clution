(eval-when
    (:compile-toplevel :load-toplevel :execute)
  (handler-bind ((error (lambda (e)
                           (format *error-output* "Error requiring ASDF:~%~T~A~%" e))))
    (require 'asdf))
  (handler-bind ((error (lambda (e)
                          (format *error-output* "Error requiring UIOP:~%~T~A~%" e))))
    (require 'uiop)))
(eval-when
    (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (progn
        (let
            ((qlfile-libs-paths
               (directory
                (merge-pathnames "qlfile-libs/**/*.asd" *load-truename*))))
          (flet
              ((clution-qlfile-libs-searcher
                   (system-name)
                 (loop :for path :in qlfile-libs-paths :if
                                                       (string-equal system-name
                                                                     (pathname-name path))
                       :return path)))
            (push #'clution-qlfile-libs-searcher asdf:*system-definition-search-functions*)))
        (let
            ((clution-systems-alist
               (mapcar
                (lambda
                    (p)
                  (cons
                   (car p)
                   (merge-pathnames
                    (cdr p)
                    *load-truename*)))
                '(("cl-clution" . "systems/cl-clution/cl-clution.asd")
                  ("asd-serializer" . "systems/asd-serializer/asd-serializer.asd")))))
          (flet
              ((clution-system-searcher
                   (system-name)
                 (loop :for
                       (name . path)
                         :in clution-systems-alist :if
                                                   (string-equal system-name name)
                       :return
                       (parse-namestring path))))
            (push #'clution-system-searcher asdf:*system-definition-search-functions*)))
        (let
            ((*standard-output*
               (make-broadcast-stream))
             (*trace-output*
               (make-broadcast-stream))
             (*error-output*
               (make-broadcast-stream)))
          (asdf:load-system "cl-clution" :verbose nil)))
    (error
      (e)
      (format *error-output* "Error loading systems:~%~T~A" e)
      (uiop:quit 1 cl:t))))
(handler-case
    (let
        ((ret-code
          (apply #'cl-clution::main
                 (uiop:raw-command-line-arguments))))
      (if
          (integerp ret-code)
          (uiop:quit ret-code cl:t)
        (uiop:quit 0 cl:t)))
  (error
   (e)
   (format *error-output* "Uncaught error:~%~T~A" e)
   (uiop:quit 1 cl:t)))
