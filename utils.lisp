(cl:defpackage :bodge-blobs-support
  (:use :cl)
  (:export register-library-directory
           register-library-system-directory
           list-registered-libraries
           load-foreign-libraries
           close-foreign-libraries
           link-system-foreign-libraries
           bodge-blob-system))
(cl:in-package :bodge-blobs-support)


(defvar *libraries* nil)


(defclass library ()
  ((name :initarg :name :reader %name-of)
   (system-name :initarg :system-name :reader %system-name-of)
   (handle :initarg :handle)))


(defun load-library (lib)
  (with-slots (handle) lib
    (cffi:load-foreign-library (cffi:foreign-library-name handle))))


(defun close-library (lib)
  (with-slots (handle) lib
    (cffi:close-foreign-library handle)))


(defun library-loaded-p (lib)
  (with-slots (handle) lib
    (cffi:foreign-library-loaded-p handle)))


(defun library-id (lib)
  (with-slots (handle) lib
    (cffi:foreign-library-name handle)))


(defun library-name (lib)
  (with-slots (name) lib
    name))


(defun register-library-directory (directory)
  (pushnew directory cffi:*foreign-library-directories* :test #'equal))


(defun register-library-system-directory (system &optional (subdirectory #p"lib/"))
  (let* ((sys-dir (asdf:component-pathname (asdf:find-system system)))
         (lib-dir (merge-pathnames subdirectory sys-dir)))
    (register-library-directory lib-dir)))


(defun library-registered-p (name)
  (member name *libraries* :test #'equal :key #'library-name))


(defun %register-libraries (&rest libraries)
  (alexandria:nconcf *libraries* libraries))


(defun find-library-absolute-path (library-name)
  (let ((search-directories (cffi::parse-directories cffi:*foreign-library-directories*)))
    (cffi::find-file library-name search-directories)))


(defun list-registered-libraries ()
  (let ((search-directories (cffi::parse-directories cffi:*foreign-library-directories*)))
    (loop for library-name in (mapcar #'library-name *libraries* )
       as library-path = (cffi::find-file library-name search-directories)
       when library-path
       collect library-path)))


(defun load-foreign-libraries ()
  (dolist (library *libraries*)
    (unless (library-loaded-p library)
      (load-library library))))


(defun close-foreign-libraries ()
  (dolist (library (reverse *libraries*))
    (when (library-loaded-p library)
      (close-library library))))


(defun link-foreign-library (name destination)
  (unless (uiop:file-exists-p destination)
    (uiop:run-program (format nil "ln -s '~A' '~A'"
                              (find-library-absolute-path name)
                              destination))))


(defun link-system-foreign-libraries (system-name destination-directory)
  (alexandria:when-let* ((system (asdf:find-system system-name))
                         (component-name (asdf:component-name system)))
    (loop for lib in *libraries*
          when (equal component-name (%system-name-of lib))
            do (link-foreign-library (%name-of lib) (format nil "~A/~A"
                                                            destination-directory
                                                            (%name-of lib))))))


(defun conc-symbols (separator &rest symbols)
  (apply #'alexandria:symbolicate
         (loop for (symbol . rest) on (alexandria:flatten symbols)
               if rest
                 append (list symbol separator)
               else
                 append (list symbol))))


(defclass asdf/interface::bodge-blob-system (asdf:system)
  ((libraries :initarg :libraries :initform nil)))


(defmethod asdf:perform :after ((operation asdf:load-op) (this asdf/interface::bodge-blob-system))
  (with-slots (libraries) this
    (labels ((feature-test-list (features)
               `(:and ,@(alexandria:ensure-list features)))
             (test-key (lib-def)
               (feature-test-list (first lib-def))))
      (alexandria:if-let ((supported-libraries (remove-if (complement #'alexandria:featurep)
                                                          libraries :key #'test-key)))
        (loop for library in supported-libraries
              do (let* ((library-name (second library))
                        (library-search-path (third library))
                        (full-search-path (asdf:system-relative-pathname this library-search-path)))
                   (unless (library-registered-p library-name)
                     (register-library-directory full-search-path)
                     (%register-libraries
                      (make-instance 'library
                                     :name library-name
                                     :system-name (asdf:component-name this)
                                     :handle (cffi:load-foreign-library
                                              library-name
                                              :search-path full-search-path))))))
        (error "No libraries found for current architecture")))))
