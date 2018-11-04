(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:shadowing-import-from #:enhanced-multiple-value-bind
                          #:multiple-value-bind)
  (:shadow #:boundp)
  (:export #:system
           #:standard-system
           #:ikeywords-mixin
           #:root-system
           #:locate
           #:unbind
           #:boundp
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
