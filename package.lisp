(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:shadowing-import-from #:enhanced-multiple-value-bind
                          #:multiple-value-bind)
  (:shadow #:boundp)
  (:export #:system
           #:systems
           #:locate
           #:unbind
           #:boundp
           #:not-found
           #:hash-table-mixin
           #:name
           #:name-mixin
           #:expand-definition
           #:define
           #:standard-system
           #:ikeywords-mixin))

(ikeywords:defpackage #:definitions-systems.ikeyword
  (:nicknames #:defsys.ikeyword)
  (:export #:operator
           #:special-form
           #:macro))
