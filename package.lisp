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
           #:check-definition-mixin
           #:base-definition-class-mixin
           #:base-definition-class
           #:unsuitable-definition-error
           #:details
           #:check-definition
           #:standard-system
           #:not-found

           #:locate
           #:unbind
           #:unbind-definition
           #:boundp
           #:ensure
           #:expand-definition
           #:define

           #:default-definition-class-mixin
           #:default-definition-class
           #:simple-expansion-mixin
           #:explicit-definition-class-p
           #:definition-class
           #:expand-definition-args

           #:root-system
           #:location-mixin
           #:location
           #:standard-root-system

           #:ikeywords-mixin))

(ikeywords:defpackage #:definitions-systems.ikeyword
  (:nicknames #:defsys.ikeyword)
  (:export #:operator
           #:special-form
           #:macro))
