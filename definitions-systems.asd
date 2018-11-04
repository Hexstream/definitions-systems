(asdf:defsystem #:definitions-systems

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "Provides a simple unified extensible way of processing named definitions."

  :depends-on (#:enhanced-multiple-value-bind
               #:incognito-keywords)

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
               (:file "main")
               (:file "ikeywords"))

  :in-order-to ((asdf:test-op (asdf:test-op #:definitions-systems_tests))))
