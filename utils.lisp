(cl:defpackage :bodge-blobs-support
  (:use :cl)
  (:export register-library-directory
           register-foreign-libraries
           list-registered-libraries
           load-foreign-libraries))
(cl:in-package :bodge-blobs-support)


(defvar *libraries* nil)


(defun register-library-directory (system &optional (subdirectory #p"lib/"))
  (let* ((sys-dir (asdf:component-pathname (asdf:find-system system)))
         (lib-dir (merge-pathnames subdirectory sys-dir)))
    (pushnew lib-dir cffi:*foreign-library-directories* :test #'equal)))


(defun %register-library-names (&rest libraries)
  (setf *libraries* (nconc *libraries* libraries)))


(defmacro register-foreign-libraries (os &rest libraries)
  `(progn
     (%register-library-names ,@libraries)
     ,@(loop for lib in libraries
          collect `(cffi:define-foreign-library ,(make-symbol lib)
                     (,os ,lib)))))


(defun list-registered-libraries ()
  (let ((search-directories (cffi::parse-directories cffi:*foreign-library-directories*)))
    (loop for library-name in *libraries*
       as library-path = (cffi::find-file library-name search-directories)
       when library-path
       collect library-path)))


(defun load-foreign-libraries ()
  (dolist (library-path (list-registered-libraries))
    (cffi:load-foreign-library library-path)))
