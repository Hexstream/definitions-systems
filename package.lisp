(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:shadow #:boundp)
  (:export #:system
           #:standard-system
           #:ikeywords-mixin
           #:root-system
           #:standard-root-system
           #:locate
           #:unbind
           #:boundp
           #:ensure

           #:not-found
           #:hash-table-mixin
           #:name-mixin
           #:name

           #:expand-definition
           #:define))

(ikeywords:defpackage #:definitions-systems.ikeyword
  (:nicknames #:defsys.ikeyword)
  (:export #:operator
           #:special-form
           #:macro))
