(in-package #:definitions-systems)

(defclass defsys:system () ())

(defclass defsys:hash-table-mixin (defsys:system)
  ((%hash :reader %hash
          :type hash-table
          :initform (make-hash-table :test 'eq))))

(defclass defsys:definition-order-mapping-mixin (defsys:system)
  ((%definition-ordered-list :reader %definition-ordered-list
                             :type %insertion-ordered-list
                             :initform (make-instance '%insertion-ordered-list
                                                      :hash-table (make-hash-table :test 'eq)))))

(defclass defsys:standard-system (defsys:base-definition-class-mixin
                                  defsys:standard-definition
                                  defsys:definition-order-mapping-mixin
                                  defsys:hash-table-mixin
                                  defsys:system)
  ())
