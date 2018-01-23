;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(defpackage #:%clution.lib
  (:use
   #:alexandria
   #:cl
   #:cl-arrows
   #:enumerable
   #:parse-float)
  (:export
   ;;;util
   #:*%eol-sequences*

   #:%whitespacep
   #:%terminating-p
   #:%cell-before
   #:%cell-after
   #:%item-before
   #:%item-after
   #:%guess-eol-style))
