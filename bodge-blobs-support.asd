(asdf:defsystem #:bodge-blobs-support
  :description "Common utilities for loading/distributing foreign libraries"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "The Unlicense"
  :defsystem-depends-on (#:trivial-features)
  :depends-on (#:uiop #:asdf #:alexandria #:cffi)
  :serial t
  :components ((:file "utils")))
