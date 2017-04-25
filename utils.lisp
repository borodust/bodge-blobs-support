(cl:defpackage :bodge-blobs-support
  (:use :cl)
  (:export register-library-directory
           register-foreign-libraries
           load-foreign-libraries))
(cl:in-package :bodge-blobs-support)


(defvar *libraries* nil)


(defun register-library-directory (system &optional (subdirectory #p"lib/"))
  (let* ((sys-dir (asdf:component-pathname (asdf:find-system system)))
         (lib-dir (merge-pathnames subdirectory sys-dir)))
    (pushnew lib-dir cffi:*foreign-library-directories* :test #'equal)))


(defmacro register-foreign-libraries (os &rest libraries)
  (setf *libraries* (nconc *libraries* libraries))
  `(progn
     ,@(loop for lib in libraries
          collect `(cffi:define-foreign-library ,(make-symbol lib)
                     (,os ,lib)))))


(defun load-foreign-libraries ()
  (let ((search-directories (cffi::parse-directories cffi:*foreign-library-directories*)))
    (dolist (library-name *libraries*)
      (let ((library-path (cffi::find-file library-name search-directories)))
        (when library-path
          (cffi:load-foreign-library library-path))))))
