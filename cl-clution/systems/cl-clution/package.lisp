(in-package #:cl-user)

(defpackage #:cl-clution
  (:use
   #:alexandria
   #:cl
   #:cl-arrows
   #:clution.lib.asd
   #:enumerable)
  (:export
   #:main))
