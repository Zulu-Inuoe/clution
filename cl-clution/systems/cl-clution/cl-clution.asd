(defsystem #:cl-clution
  :name "cl-clution"
  :version "0.0.0"
  :description "clution environment in CL"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:static-file "qlfile")
   (:file "package")
   (:file "cl-clution"))
  :depends-on
  (#:alexandria
   #:enumerable
   #:cl-arrows
   #:clution.lib))
