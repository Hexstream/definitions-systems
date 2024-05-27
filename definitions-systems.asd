(asdf:defsystem #:definitions-systems

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "Provides a simple unified extensible way of processing named definitions."

  :depends-on ("canonicalized-initargs"
               "enhanced-defclass"
               "shared-preferences"
               "enhanced-find-class"
               "closer-mop")

  :version "4.0"
  :serial cl:t
  :components ((:file "package")
               (:file "insertion-ordered-list")
               (:file "generic-functions")
               (:file "classes")
               (:file "frontend")
               (:file "forwarding")
               (:file "primary-binding-mixin")
               (:file "alias-bindings-mixin")
               (:file "backend")
               (:file "checking")
               (:file "hash-table-mixin")
               (:file "definition-order-mapping-mixin")
               (:file "root")
               (:file "expansion"))

  :in-order-to ((asdf:test-op (asdf:test-op #:definitions-systems_tests))))
