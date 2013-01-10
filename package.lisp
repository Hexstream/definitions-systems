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
           #:expand-definition
           #:define))

(cl:defpackage #:definitions-systems-proxy
  (:nicknames #:defsys-proxy)
  (:use #:cl)
  (:export #:standard-proxy))

(ikeywords:defpackage #:definitions-systems.ikeyword
  (:nicknames #:defsys.ikeyword)
  (:export #:operator
           #:special-form
           #:macro))

(cl:defpackage #:definitions-systems-cl
  (:use #:cl)
  (:import-from #:definitions-systems #:define)
  (:documentation "An internal package for defining definitions-systems for CL (proxies)."))
