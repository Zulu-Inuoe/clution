;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defsystem #:clution.lib
  :version "0.0.0"
  :description "Project development tools for CL."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "%package")
   (:file "util")
   (:module "cl-sexp"
    :components
    ((:file "package")
     (:file "parser")
     (:file "lexer")))
   (:module "asd"
    :components
    ((:file "package")
     (:file "asd")))
   (:module "clu"
    :components
    ((:file "package")
     (:file "clu")))
   (:file "package")
   (:file "clution"))
  :depends-on
  (#:alexandria
   #:cl-arrows
   #:enumerable
   #:parse-float
   #:qlot
   #:qlot/install
   #:qlot/source
   #:qlot/source/git
   #:qlot/source/github
   #:qlot/source/http
   #:qlot/source/ql
   #:qlot/parser
   #:trivial-features))
