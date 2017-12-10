(cl:defpackage :bodge-blobs-support
  (:use :cl)
  (:export register-library-directory
           register-library-system-directory
           define-foreign-library-collection
           list-registered-libraries
           load-foreign-libraries
           close-foreign-libraries))
(cl:in-package :bodge-blobs-support)


(defvar *libraries* nil)


(defun register-library-directory (directory)
  (pushnew directory cffi:*foreign-library-directories* :test #'equal))


(defun register-library-system-directory (system &optional (subdirectory #p"lib/"))
  (let* ((sys-dir (asdf:component-pathname (asdf:find-system system)))
         (lib-dir (merge-pathnames subdirectory sys-dir)))
    (register-library-directory lib-dir)))


(defun %register-library-names (libraries)
  (flet ((already-exists (el)
           (member (symbol-name el) *libraries* :test #'equal :key #'symbol-name)))
    (setf *libraries* (nconc *libraries* (remove-if #'already-exists libraries)))))


(defmacro define-foreign-library-collection (os system-library-directory &rest libraries)
  (let ((lib-names (loop for lib in libraries
                      collect (make-symbol lib))))
    (destructuring-bind (system &optional (path "lib/"))
        (alexandria:ensure-list system-library-directory)
      `(progn
         (register-library-system-directory ,system ,path)
         (%register-library-names '(,@lib-names))
         ,@(loop for lib in lib-names
              collect `(cffi:define-foreign-library
                           ,lib
                         (,os ,(symbol-name lib))))))))


(defun list-registered-libraries ()
  (let ((search-directories (cffi::parse-directories cffi:*foreign-library-directories*)))
    (loop for library-name in *libraries*
       as library-path = (cffi::find-file (symbol-name library-name) search-directories)
       when library-path
       collect library-path)))


(defun load-foreign-libraries ()
  (dolist (library *libraries*)
    (unless (cffi:foreign-library-loaded-p library)
      (cffi:load-foreign-library library))))


(defun close-foreign-libraries ()
  (dolist (library (reverse *libraries*))
    (cffi:close-foreign-library library)))
