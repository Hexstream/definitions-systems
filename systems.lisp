(in-package #:definitions-systems)

(defclass defsys:system () ())

(defclass defsys:hash-table-mixin ()
  ((%hash :reader %hash
          :type hash-table
          :initform (make-hash-table :test 'eq))))

(defclass defsys:standard-system (defsys:system
                                  defsys:base-definition-class-mixin
                                  defsys:standard-definition
                                  defsys:hash-table-mixin)
  ())
