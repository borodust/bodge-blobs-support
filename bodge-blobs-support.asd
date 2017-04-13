(cl:defpackage :bodge-blobs-support.def
  (:use :cl :asdf))

(cl:in-package :bodge-blobs-support.def)


(defsystem bodge-blobs-support
  :description "Common utilities for loading/distributing foreign libraries"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "The Unlicense"
  :depends-on (cffi)
  :serial t
  :components ((:file "utils")))
