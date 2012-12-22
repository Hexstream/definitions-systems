(asdf:defsystem #:definitions-systems

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "To be described."

  :depends-on (#:incognito-keywords)

  :version "0.0"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
