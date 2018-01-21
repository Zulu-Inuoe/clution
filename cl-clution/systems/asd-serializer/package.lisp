;;;asd-serializer - read/writer for asdf asd files
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defpackage #:asd-serializer
  (:use
   #:alexandria
   #:cl
   #:cl-arrows
   #:enumerable
   #:parse-float)
  (:export
   #:read-asd-file
   #:write-asd-file

   #:asd-file-systems

   ;;; System
   #:system-name
   #:system-long-name
   #:system-version
   #:system-description
   #:system-long-description
   #:system-author
   #:system-license
   #:system-pathname
   #:system-serial
   #:system-components
   #:system-depends-on

   #:system-plist

   ;; component CRUD
   #:system-add-file-component
   #:system-add-module-component
   #:system-rename-component
   #:system-move-component-up
   #:system-move-component-down
   #:system-remove-component

   ;; system depends-on CRUD
   #:system-add-depends-on
   #:system-remove-depends-on))
