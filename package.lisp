(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:shadowing-import-from #:enhanced-multiple-value-bind
                          #:multiple-value-bind)
  (:shadow #:boundp)
  (:export #:system
           #:nil-to-not-found-mixin
           #:systems
           #:locate
           #:unbind
           #:boundp
           #:not-found
           #:not-found-class
           #:locator
           #:hash-table-mixin
           #:name
           #:name-mixin
           #:make
           #:make-and-bind
           #:ensure
           #:define))

(ikeywords:defpackage #:definitions-systems.ikeyword
  (:nicknames #:defsys.ikeyword)
  (:export #:operator
           #:special-form
           #:macro))
