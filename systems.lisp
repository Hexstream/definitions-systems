(in-package #:definitions-systems)

(defclass defsys:system () ())

(defclass defsys:hash-table-mixin ()
  ((%hash :type hash-table :initform (make-hash-table :test 'eq))))

(defclass defsys:standard-system (defsys:system
                                  defsys:base-definition-class-mixin
                                  defsys:standard-definition
                                  defsys:hash-table-mixin)
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
