(defsystem #:cl-clution
  :name "cl-clution"
  :version "0.0.0"
  :description "clution environment in CL"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 <http://creativecommons.org/publicdomain/zero/1.0/>"
  :serial t
  :components
  ((:file "package")
   (:file "cl-clution"))
  :depends-on
  (#:asd-serializer
   #:alexandria
   #:enumerable))
