(cl:defpackage #:definitions-systems
  (:nicknames #:defsys)
  (:use #:cl)
  (:shadowing-import-from #:enhanced-defclass #:defclass)
  (:shadowing-import-from #:enhanced-find-class #:find-class)
  (:shadow #:map
           #:count)
           ;;; generic-functions.lisp
  (:export #:locate
           #:unbind
           #:map
           #:count

           #:owner
           #:name

           #:bind-definition
           #:unbind-definition
           #:replace-definition

           #:ensure
           #:default-system
           #:expand

           ;;; classes.lisp
           #:definition
           #:standard-definition

           #:system
           #:standard-system

           #:not-found

           ;;; bindings.lisp
           #:owner-mixin
           #:name-mixin
           #:primary-binding-mixin

           #:alias-bindings-mixin
           #:map-aliasing-systems

           ;;; checking.lisp
           #:check-definition-mixin
           #:check-definition

           #:base-definition-class-mixin
           #:base-definition-class

           #:unsuitable-definition-error
           #:details

           ;;; hash-table-mixin.lisp
           #:hash-table-mixin

           ;;; definition-order-mapping-mixin.lisp
           #:definition-order-mapping-mixin

           ;;; root.lisp
           #:root-system
           #:location-mixin
           #:location
           #:standard-root-system

           ;;; expansion.lisp
           #:define))

(setf (shared-preferences:preferences-1 (find-package '#:definitions-systems))
      (make-instance 'enhanced-defclass:preferences
                     'enhanced-defclass:default-metaclass-advisor (find-class 'canon-initargs:standard-class)))
