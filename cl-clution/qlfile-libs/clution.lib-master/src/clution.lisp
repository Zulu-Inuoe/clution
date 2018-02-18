;;;clution.lib - project development tools for CL
;;;Written in 2018 by Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>
;;;
;;;To the extent possible under law, the author(s) have dedicated all copyright
;;;and related and neighboring rights to this software to the public domain
;;;worldwide. This software is distributed without any warranty.
;;;You should have received a copy of the CC0 Public Domain Dedication along
;;;with this software. If not, see
;;;<http://creativecommons.org/publicdomain/zero/1.0/>.

(in-package #:clution.lib)

(defun %asd-file-system-qlfile-component (system)
  "Get the qlfile component of `system', or nil if system does not have a qlfile"
  (-> (asd-file-system-components system)
      (where (lambda (c) (typep c 'asd-file-system-static-file-component)))
      (where (lambda (c) (string= (asd-file-system-component-name c) "qlfile")))
      (efirst)))

(defun %concatenate-qlfiles (qlfile-paths stream)
  "Concatenate the contents of each qlfile designated in `qlfile-paths' to `stream'"
  (do-enumerable (qlfile qlfile-paths)
    (with-input-from-file (qlfile-stream qlfile)
      (copy-stream qlfile-stream stream)
      (fresh-line stream)))
  (terpri stream))

(defun %output-concatenated-qlfile (qlfile-paths pathname)
  "Concatenate each of, outputting the result to `stream'"
  (with-output-to-file (concatenated-qlfile pathname :if-exists :supersede)
    (%concatenate-qlfiles qlfile-paths concatenated-qlfile)))

(defun %copy-qlfiles-fetch (fetch-dir libs-dir)
  ;;Copy to libs-dir
  ;;Delete the qlfile libs currently there
  (uiop:delete-directory-tree libs-dir :validate t :if-does-not-exist :ignore)

  ;;Copy over the fetched contents
  (ensure-directories-exist libs-dir)
  (let ((fetch-dists-dir (%pathname-as-directory (%path-combine fetch-dir "quicklisp" "dists"))))
    (do-enumerable (dist-dir (-> (uiop:subdirectories fetch-dists-dir)
                                 ;;skip the quicklisp dist
                                 (where (lambda (d) (string/= (%directory-name d) "quicklisp")))
                                 ;;
                                 (select-many (lambda (d) (uiop:subdirectories (%pathname-as-directory (%path-combine d "software")))))))
      (%copy-directory dist-dir libs-dir))))

(defun %fetch-qlfiles (qlfile-paths fetch-dir
                       &aux
                         (fetch-qlfile-path (%expand-pathname "qlfile" fetch-dir))
                         (fetch-quicklisp-dir (%pathname-as-directory
                                               (%expand-pathname "quicklisp" fetch-dir))))
  ;;Merge the qlfiles
  (ensure-directories-exist fetch-dir)
  (%output-concatenated-qlfile qlfile-paths fetch-qlfile-path)

  ;;Ensure there's no conflicting sources
  (let ((sources (select (qlot/parser:parse-qlfile fetch-qlfile-path)
                         (lambda (s)
                           (qlot/source:prepare s)
                           s))))
    (when-let (sources-with-conflicts
               (to-list
                (where sources
                       (lambda (s1)
                         (any* sources
                               (lambda (s2)
                                 (and (string= (qlot/source:source-dist-name s1)
                                               (qlot/source:source-dist-name s2))
                                      (string/= (qlot/source:source-version s1)
                                                (qlot/source:source-version s2)))))))))
      (error "clution: conflicting versions for sources: ~{'~A'~^,~}"
             (mapcar #'qlot/source:source-dist-name sources-with-conflicts))))
  ;;Install the qlfile
  (qlot/install:install-qlfile fetch-qlfile-path))

(defun %sync-qlfiles (qlfile-paths fetch-dir libs-dir)
  ;;Do fetch
  (%fetch-qlfiles qlfile-paths fetch-dir)
  ;;Copy to libs
  (%copy-qlfiles-fetch fetch-dir libs-dir))

(defun %sync-asd-qlfiles (asd-files fetch-dir libs-dir)
  "Sync the qlfiles of each system in the given `asd-files',
using `fetch-dir' as the temporary fetch directory and outputting to `libs-dir'"
  (-> asd-files
      (select-many #'asd-file-systems)
      (select #'%asd-file-system-qlfile-component)
      (where #'identity) ; not all systems will have a qlfile
      (select #'asd-file-system-component-path)
      (%sync-qlfiles fetch-dir libs-dir)))

(defun %clu-qlfiles (clu-file)
  (-> (clu-file-systems clu-file)
      (select #'clu-file-system-item-path)
      (select #'read-asd-file)
      (select-many #'asd-file-systems)
      (select #'%asd-file-system-qlfile-component)
      (where #'identity)
      (select #'asd-file-system-component-path)))

(defun clu-has-qlfiles (clu-file)
  (any (%clu-qlfiles clu-file)))

(defun clu-qlfile-libs-up-to-date (clu-file)
  (let* ((qlfiles (%clu-qlfiles clu-file))
         (qlfile-libs-dir (clu-file-qlfile-libs-dir clu-file))
         (qlfile-fetch-dir (clu-file-qlfile-fetch-dir clu-file))
         (qlfile-fetch-qlfile (%expand-pathname "qlfile" qlfile-fetch-dir)))
    (or (not (any qlfiles))
        (and (uiop:directory-exists-p qlfile-libs-dir)
             (uiop:directory-exists-p qlfile-fetch-dir)
             (uiop:file-exists-p qlfile-fetch-qlfile)
             (all qlfiles
                  (lambda (qlfile)
                    (and (uiop:file-exists-p qlfile)
                         (uiop:timestamp<
                          (uiop:safe-file-write-date qlfile)
                          (uiop:safe-file-write-date qlfile-fetch-qlfile)))))
             t))))

(defun clu-sync-qlfiles (clu-file)
  "Sync the qlfiles of systems in `clu-file'"
  (-> (clu-file-systems clu-file)
      (select #'clu-file-system-item-path)
      (select #'read-asd-file)
      (%sync-asd-qlfiles (clu-file-qlfile-fetch-dir clu-file) (clu-file-qlfile-libs-dir clu-file))))
