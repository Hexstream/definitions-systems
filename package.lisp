(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:export #:system
           #:nil-to-not-found-mixin
           #:systems
           #:locate
           #:unbind
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
