(asdf:defsystem #:definitions-systems

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "Provides a simple unified extensible way of processing named definitions."

  :depends-on ("canonicalized-initargs"
               "enhanced-defclass"
               "shared-preferences"
               "enhanced-find-class"
               "closer-mop")

  :version "3.0"
  :serial cl:t
  :components ((:file "package")
               (:file "definitions")
               (:file "systems")
               (:file "checking")
               (:file "root")
               (:file "protocols")
               (:file "forwarding")
               (:file "hash-table-mixin"))

  :in-order-to ((asdf:test-op (asdf:test-op #:definitions-systems_tests))))
