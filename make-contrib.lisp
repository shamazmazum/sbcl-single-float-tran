(setf (sb-ext:readtable-base-char-preference *readtable*) :both)
(defvar *system* (second sb-ext:*posix-argv*))
;; Convert dep filenames into module names. Depending on whether each Make step
;; saw the fasls as already-existing, or is making them, the dep is either the
;; unadorned fasl name, or containing the 'vpath' in it already.
(defvar *deps*
  (mapcar 'string-upcase (mapcar 'pathname-name (cddr sb-ext:*posix-argv*))))
(format t "; Note: Building ~S~@[, deps=~S~]~%" *system* *deps*)
(mapc 'require *deps*)

(defun logicalize (path generated)
  (assert (not generated))
  (let ((pathname (pathname path)))
    (make-pathname :directory (pathname-directory pathname)
                   :name      (pathname-name      pathname)
                   :type      "lisp")))

(defun perform (defsystem)
  (when (member :sb-cover-for-internals sb-impl:+internal-features+)
    (proclaim '(optimize sb-c::store-coverage-data)))
  (let* ((specified-sources (getf defsystem :components))
         ;; This path is basically arbitrary. I wanted to avoid creating
         ;; another directory under "obj/" but alas ...
         (objdir (format nil "obj/from-self/contrib/~A/" *system*))
         (*features* (append '(:sb-building-contrib) *features*
                             sb-impl:+internal-features+)))
    (ensure-directories-exist objdir)
    ;; For source locations.
    ;; Even though generated files are not shipped as sources it's
    ;; better to hide the original pathnames.
    (push (list "SYS:OBJ;**;*.*.*"
                (merge-pathnames "**/*.*" (truename "obj/")))
          (logical-pathname-translations "SYS"))
    (sb-int:collect ((alien-constants) (flattened-sources) (fasls))
      (let ((runtime-deps *deps*))
        (when runtime-deps
          (let ((fasl (sb-c:compile-form-to-file
                       `(progn ,@(mapcar (lambda (x) `(require ,(string x))) runtime-deps))
                       (merge-pathnames "module-setup" objdir))))
            (flattened-sources `(t ,fasl)))))
      ;; Compile all files serially. :depends-on is just documentation for the user
      (sb-int:named-let flatten ((prefix "") (sources specified-sources))
        (dolist (source sources)
          (ecase (car source)
            (:module
             (let* ((subdir (cadr source))
                    (pathname (getf source :pathname subdir))
                    (newprefix (if (string= pathname  "")
                                   prefix
                                   (concatenate 'string prefix subdir "/"))))
               (unless (string= pathname "")
                 (ensure-directories-exist (format nil "~A~A" objdir newprefix)))
               (flatten newprefix (getf source :components))))
            (:file
             (let ((if-feature (getf source :if-feature)))
               (when (or (not if-feature) (sb-int:featurep if-feature))
                 (flattened-sources
                  `(nil ,(concatenate 'string prefix (cadr source)))))))
            (:sb-grovel-constants-file
             (error "I don't require this")))))
      (let ((fasl (sb-c:compile-form-to-file
                   `(provide ,(string-upcase *system*))
                   (merge-pathnames "module-provide" objdir))))
        (flattened-sources `(t ,fasl)))
      (when (alien-constants)
        (error "Alien constants? WTF?"))
      (let (wcu-warnings)
        (handler-bind (((and warning (not style-warning))
                        (lambda (c)
                          (push c wcu-warnings))))
          (with-compilation-unit ()
            (loop for (generated-p stem) in (flattened-sources)
             do (let ((fasl
                       (if (string= (pathname-type stem) "fasl")
                           stem
                           (multiple-value-bind (output warnings errors)
                               (compile-file (logicalize stem generated-p)
                                             :output-file
                                             (format nil "~A~A.fasl" objdir stem))
                             (when (or warnings errors) (sb-sys:os-exit 1))
                             output))))
                  (fasls fasl)
                  (load fasl)))))
        ;; Deferred warnings occur *after* exiting the W-C-U body.
        ;; See also lp#1078460 - "unknown variable" is not really ever resolved.
        (when wcu-warnings (sb-sys:os-exit 1)))
      (let ((outputs (mapcar 'namestring (fasls)))
            (joined (format nil "obj/sbcl-home/contrib/~A.fasl" *system*)))
        (ensure-directories-exist joined)
        (with-open-file (asd (merge-pathnames (make-pathname :type "asd") joined)
                             :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (format asd "(defsystem :~A :class require-system)~%" *system*))
        (sb-sys:os-exit
         (process-status
          ;; for #+win32 it's probably /usr/bin/cat.exe,
          ;; for #+unix it's supposed to be /bin/cat, but lp#1995224 says otherwise
          (run-program "cat" outputs :search t
                       :output joined :if-output-exists :supersede)))))))

(compile 'perform)
(let ((form (with-open-file (f (format nil "~A.asd" *system*))
              (let ((form (read f)))
                ;; each .asd file has an ERROR form preventing users from LOADing it
                (assert (eq (car form) 'error))
                (read f)))))
  (let ((eval (getf form :eval)))
    (when eval (eval eval)))
  (let ((bindings (getf form :bind))
        (*compile-verbose* nil)) ; set the default
    (progv (mapcar 'first bindings) (mapcar 'second bindings)
      (perform form))))
