;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defpackage #:clution.lib.cl-sexp
  (:use
   #:%clution.lib
   #:alexandria
   #:cl
   #:cl-arrows
   #:enumerable
   #:parse-float)
  (:export
   #:sexp-node
   #:sexp-node-parent

   #:sexp-opaque-node
   #:opaque-node-text
   #:%opaque-node-p

   #:sexp-whitespace-node
   #:%whitespace-node-p

   #:sexp-parent-node
   #:children
   #:%parent-node-p
   #:vchildren

   #:sexp-list-node
   #:%list-node-p

   #:sexp-list-nth
   #:sexp-list-getf

   #:sexp-vector-node

   #:token-text
   #:sexp-symbol-node
   #:symbol-node-name
   #:symbol-node-package
   #:symbol-node-external-ref-p
   #:%symbol-node-p
   #:sexp-symbol-match

   #:sexp-string-node
   #:string-node-string
   #:%string-node-p


   #:%parse-sexp-file
   #:%write-node))
