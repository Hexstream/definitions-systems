(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:shadow #:boundp
           #:map)
  (:export #:definition
           #:name-mixin
           #:name
           #:owner-mixin
           #:owner
           #:standard-definition

           #:system
           #:hash-table-mixin
           #:standard-system
           #:not-found

           #:check-definition-mixin
           #:base-definition-class-mixin
           #:base-definition-class
           #:unsuitable-definition-error
           #:details
           #:check-definition

           #:locate
           #:unbind
           #:unbind-definition
           #:boundp
           #:ensure
           #:expand
           #:define
           #:map

           #:default-definition-class-mixin
           #:default-definition-class
           #:simple-expansion-mixin
           #:explicit-definition-class-p
           #:definition-class
           #:expand-args

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
