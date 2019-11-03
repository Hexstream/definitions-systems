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
           #:standard-system
           #:ikeywords-mixin
           #:root-system
           #:standard-root-system
           #:locate
           #:unbind
           #:unbind-definition
           #:boundp
           #:ensure

           #:not-found
           #:hash-table-mixin

           #:expand-definition
           #:define))

(ikeywords:defpackage #:definitions-systems.ikeyword
  (:nicknames #:defsys.ikeyword)
  (:export #:operator
           #:special-form
           #:macro))
