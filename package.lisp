(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:shadowing-import-from #:enhanced-defclass #:defclass)
  (:shadowing-import-from #:enhanced-find-class #:find-class)
  (:shadow #:map
           #:count)
  (:export #:definition
           #:name-mixin
           #:name
           #:owner-mixin
           #:owner
           #:primary-binding-mixin
           #:alias-bindings-mixin
           #:map-aliasing-systems
           #:standard-definition

           #:system
           #:hash-table-mixin
           #:definition-order-mapping-mixin
           #:standard-system
           #:not-found

           #:check-definition-mixin
           #:base-definition-class-mixin
           #:base-definition-class
           #:unsuitable-definition-error
           #:details
           #:check-definition

           #:locate
           #:bind-definition
           #:replace-definition
           #:unbind
           #:unbind-definition
           #:ensure
           #:default-system
           #:expand
           #:define
           #:map
           #:count

           #:root-system
           #:location-mixin
           #:location
           #:standard-root-system))

(setf (shared-preferences:preferences-1 (find-package '#:definitions-systems))
      (make-instance 'enhanced-defclass:preferences
                     'enhanced-defclass:default-metaclass-advisor (find-class 'canon-initargs:standard-class)))
