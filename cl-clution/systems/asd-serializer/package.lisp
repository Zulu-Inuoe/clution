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

   #:asd-file-system-plists
   #:asd-file-add-file-component
   #:asd-file-add-module-component
   #:asd-file-rename-component
   #:asd-file-remove-component
   #:asd-file-add-depends-on
   #:asd-file-remove-depends-on))
