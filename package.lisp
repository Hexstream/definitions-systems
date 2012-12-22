(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:export #:system
           #:systems
           #:locate
           #:not-found
           #:locator
           #:hash-table-mixin
           #:name
           #:name-mixin
           #:ensure
           #:define))

(ikeywords:defpackage #:definitions-systems.ikeyword
  (:nicknames #:defsys.ikeyword)
  (:export #:operator
           #:special-form
           #:macro))
