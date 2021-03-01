(asdf:defsystem #:definitions-systems_tests

  :author "Jean-Philippe Paradis <hexstream@hexstreamsoft.com>"

  :license "Unlicense"

  :description "definitions-systems unit tests."

  :depends-on ("definitions-systems"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:definitions-systems_tests)))
