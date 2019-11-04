(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:shadow #:boundp)
  (:export #:definition
           #:name-mixin
           #:name
           #:owner-mixin
           #:owner
           #:standard-definition

           #:system
           #:hash-table-mixin
           #:base-definition-class-mixin
           #:base-definition-class
           #:unsuitable-definition-error
           #:details
           #:check-definition
           #:standard-system
           #:root-system
           #:standard-root-system
           #:not-found

           #:locate
           #:unbind
           #:unbind-definition
           #:boundp
           #:ensure
           #:expand-definition
           #:define

           #:ikeywords-mixin))

(ikeywords:defpackage #:definitions-systems.ikeyword
  (:nicknames #:defsys.ikeyword)
  (:export #:operator
           #:special-form
           #:macro))
