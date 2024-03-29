(uiop:define-package :bodge-blobs-support
  (:use :cl)
  (:export #:register-library-directory
           #:register-library-system-directory
           #:list-registered-libraries
           #:load-foreign-libraries
           #:close-foreign-libraries
           #:link-system-foreign-libraries
           #:bodge-blob-system-p
           #:find-loaded-library-name
           #:find-system-libraries-by-features

           #:library-descriptor-name
           #:library-descriptor-search-path))
(cl:in-package :bodge-blobs-support)


(eval-when (:execute :load-toplevel :compile-toplevel)
  (when (member :arm64 *features*)
    (pushnew :aarch64 *features*)))


(defvar *libraries* nil)


(defclass library ()
  ((name :initarg :name :reader %name-of)
   (system-name :initarg :system-name :reader %system-name-of)
   (handle :initarg :handle :initform nil)
   (nickname :initarg :nickname :initform nil :reader %nickname-of)))


(defun lookup-library (name)
  (or (loop for path in cffi:*foreign-library-directories*
            for libdir = (uiop:ensure-directory-pathname path)
              thereis (uiop:probe-file* (merge-pathnames name libdir)))
      name))


(defun load-library (lib)
  (with-slots (name handle) lib
    (setf handle (cffi:load-foreign-library (lookup-library name)))))


(defun close-library (lib)
  (with-slots (handle) lib
    #+ccl
    (flet ((remove-library-eep (name eep)
             (declare (ignore name))
             (when (eq (cffi::foreign-library-handle handle) (ccl::eep.container eep))
               (setf (ccl::eep.container eep) nil))))
      ;; FIXME HAX: ccl caches SHLIB pointer in EEP while SHLIB caches initial name/soname
      ;; so on reloading it will try to use this old cached SHLIB full path failing
      ;; to find the lib on different systems
      (maphash #'remove-library-eep (ccl::eeps)))
    (cffi:close-foreign-library handle)))


(defun library-loaded-p (lib)
  (with-slots (handle) lib
    (when handle
      (cffi:foreign-library-loaded-p handle))))


(defun library-id (lib)
  (with-slots (handle) lib
    (cffi:foreign-library-name handle)))


(defun library-name (lib)
  (with-slots (name) lib
    name))


(defun library-system (lib)
  (with-slots (system-name) lib
    system-name))


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
  (dolist (library *libraries*)
    (when (library-loaded-p library)
      (close-library library))))


(defun link-foreign-library (name destination)
  (uiop:run-program (format nil "ln -fs '~A' '~A'"
                            (find-library-absolute-path name)
                            destination)))


(defun conc-symbols (separator &rest symbols)
  (apply #'alexandria:symbolicate
         (loop for (symbol . rest) on (alexandria:flatten symbols)
               if rest
                 append (list symbol separator)
               else
                 append (list symbol))))


(defclass library-descriptor ()
  ((name :initarg :name :reader library-descriptor-name)
   (feature-expression :initarg :feature-expression :reader library-descriptor-feature-expression)
   (search-path :initarg :search-path :reader library-descriptor-search-path)
   (nickname :initarg :nickname :initform nil :reader library-descriptor-nickname)))


(defclass asdf/interface::bodge-blob-system (asdf:system)
  ((libraries :initform nil)
   (preload :initarg :preload :initform t)))


(defmethod reinitialize-instance :after ((this asdf/interface::bodge-blob-system)
                                         &key libraries)
  (with-slots ((this-libraries libraries)) this
    (setf
     this-libraries
     (loop for lib in libraries
           collect (destructuring-bind (test library-name library-search-path
                                        &key nickname &allow-other-keys)
                       lib
                     (make-instance 'library-descriptor
                                    :name library-name
                                    :feature-expression `(:and
                                                          ,@(alexandria:ensure-list test))
                                    :search-path (asdf:system-relative-pathname this
                                                                                library-search-path)
                                    :nickname nickname))))))


(defgeneric find-system-libraries-by-features (bodge-blob-system &optional features)
  (:method (bodge-blobs-system &optional (features *features*))
    (alexandria:if-let (system (ignore-errors
                                (asdf:find-system bodge-blobs-system)))
      (find-system-libraries-by-features system features)
      (call-next-method))))


(defmethod find-system-libraries-by-features ((this asdf/interface::bodge-blob-system)
                                              &optional (features *features*))
  (with-slots (libraries) this
    (labels ((featurep (feature-expression)
               (uiop:featurep feature-expression features)))
      (remove-if (complement #'featurep) libraries :key #'library-descriptor-feature-expression))))


(defmethod asdf:perform :after ((operation asdf:load-op) (this asdf/interface::bodge-blob-system))
  (with-slots (libraries preload) this
    (alexandria:if-let ((supported-libraries (find-system-libraries-by-features this)))
      (loop for library in supported-libraries
            for library-name = (library-descriptor-name library)
            for library-search-path = (library-descriptor-search-path library)
            for library-nickname = (library-descriptor-nickname library)
            do (unless (library-registered-p library-name)
                 (register-library-directory library-search-path)
                 (let ((lib (make-instance 'library
                                           :name library-name
                                           :system-name (asdf:component-name this)
                                           :nickname library-nickname)))
                   (when preload
                     (load-library lib))
                   (%register-libraries lib))))
      (error "No libraries found for current architecture"))))


(defun bodge-blob-system-p (system)
  (values (subtypep (class-of system ) 'asdf/interface::bodge-blob-system)))


(defun link-system-foreign-libraries (system-name destination-directory)
  (alexandria:when-let* ((system (asdf:find-system system-name))
                         (component-name (asdf:component-name system)))
    (let ((absolute-dest-dir (if (uiop:relative-pathname-p destination-directory)
                                 (asdf:system-relative-pathname system-name destination-directory)
                                 destination-directory)))
      (loop for lib in *libraries*
            when (equal component-name (%system-name-of lib))
              do (link-foreign-library (%name-of lib) (format nil "~A/~A"
                                                              absolute-dest-dir
                                                              (%name-of lib))))
      (loop for dependency-name in (asdf:system-depends-on system)
            as dependency = (asdf:find-system dependency-name)
            when (bodge-blob-system-p dependency)
              do (link-system-foreign-libraries dependency-name absolute-dest-dir)))))


(defun find-loaded-library-name (system-name library-nickname)
  (alexandria:when-let* ((system (asdf:find-system system-name))
                         (component-name (asdf:component-name system)))
    (loop for library in *libraries*
            thereis (and (equal (%system-name-of library) component-name)
                         (equal (%nickname-of library) library-nickname)
                         (%name-of library)))))
