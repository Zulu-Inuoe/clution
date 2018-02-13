;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defpackage #:clution.lib.asd
  (:use
   #:%clution.lib
   #:alexandria
   #:cl
   #:cl-arrows
   #:clution.lib.cl-sexp
   #:enumerable
   #:parse-float)
  (:export
   #:asd-file-system-component
   #:asd-file-system-component-name
   #:asd-file-system-component-path

   #:asd-file-system-file-component
   #:asd-file-system-module-component
   #:asd-file-system-static-file-component

   #:asd-file-system
   #:asd-file-system-components

   #:asd-file
   #:asd-file-systems
   #:read-asd-file
   #:write-asd-file

   #:asd-file-system-plists
   #:asd-file-add-file-component
   #:asd-file-add-module-component
   #:asd-file-add-static-file-component
   #:asd-file-rename-component
   #:asd-file-move-component-up
   #:asd-file-move-component-down
   #:asd-file-remove-component
   #:asd-file-add-depends-on
   #:asd-file-remove-depends-on))
