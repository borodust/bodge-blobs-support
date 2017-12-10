(asdf:defsystem bodge-blobs-support
  :description "Common utilities for loading/distributing foreign libraries"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "The Unlicense"
  :depends-on (cffi)
  :serial t
  :components ((:file "utils")))
