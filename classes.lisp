(in-package #:definitions-systems)

(defclass defsys:definition () ())

(defclass defsys:standard-definition (defsys:primary-binding-mixin
                                      defsys:alias-bindings-mixin
                                      defsys:definition)
  ())


(defclass defsys:system () ())

(defclass defsys:standard-system (defsys:base-definition-class-mixin
                                  defsys:standard-definition
                                  defsys:definition-order-mapping-mixin
                                  defsys:hash-table-mixin
                                  defsys:system)
  ())


(define-condition defsys:not-found (error)
  ((%system :initarg :system
            :reader defsys:system
            :type defsys:system)
   (%name :initarg :name
          :reader defsys:name))
  (:report (lambda (condition stream)
             (format stream "No definition named ~S in system ~S."
                     (defsys:name condition) (defsys:system condition)))))
