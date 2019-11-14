(in-package #:definitions-systems)

(defclass defsys:definition () ())

(defgeneric defsys:name (definition))

(defclass defsys:name-mixin ()
  ((%name :initarg :name
          :reader defsys:name
          :type symbol
          :initform nil)))

(defmethod print-object ((mixin name-mixin) stream)
  (print-unreadable-object (mixin stream :type t)
    (prin1 (defsys:name mixin) stream)))

(defclass defsys:owner-mixin ()
  ((%owner :reader defsys:owner
           :type (or null defsys:system)
           :initform nil)))

(defmethod make-load-form ((definition defsys:owner-mixin) &optional environment)
  (let ((owner (defsys:owner definition)))
    (if owner
        (multiple-value-bind (owner-locate owner-init) (make-load-form owner environment)
          (values `(defsys:locate ,owner-locate (defsys:name definition))
                  owner-init))
        (error "Don't know how to ~S for definition ~S because it has no owner."
               'make-load-form definition))))

(defclass defsys:standard-definition (defsys:definition defsys:name-mixin defsys:owner-mixin) ())
