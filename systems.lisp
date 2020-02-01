(in-package #:definitions-systems)

(defclass defsys:system () ())

(defclass defsys:hash-table-mixin (defsys:system)
  ((%hash :reader %hash
          :type hash-table
          :initform (make-hash-table :test 'eq))))

(defclass defsys:standard-system (defsys:base-definition-class-mixin
                                  defsys:standard-definition
                                  defsys:hash-table-mixin
                                  defsys:system)
  ())
