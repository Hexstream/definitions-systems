(in-package #:definitions-systems)

(defclass defsys:definition () ())

(defgeneric defsys:name (definition))

(defclass defsys:name-mixin ()
  ((%name :reader defsys:name
          :writer (setf %name)
          :type symbol
          :initform nil)))

(defmethod print-object ((mixin name-mixin) stream)
  (print-unreadable-object (mixin stream :type t)
    (prin1 (defsys:name mixin) stream)))

(defgeneric defsys:owner (definition))

(defclass defsys:owner-mixin ()
  ((%owner :reader defsys:owner
           :writer (setf %owner)
           :type (or null defsys:system)
           :initform nil)))

(defmethod make-load-form ((definition defsys:owner-mixin) &optional environment)
  (let ((owner (defsys:owner definition)))
    (if owner
        (multiple-value-bind (owner-locate owner-init) (make-load-form owner environment)
          (values `(defsys:locate ,owner-locate ',(defsys:name definition))
                  owner-init))
        (error "Don't know how to ~S for definition ~S because it has no owner."
               'make-load-form definition))))

(defclass defsys:primary-binding-mixin (defsys:owner-mixin defsys:name-mixin defsys:definition)
  ())

(defmethod shared-initialize :after ((definition defsys:primary-binding-mixin) slot-names
                                     &key (owner nil owner-supplied-p) (name nil name-supplied-p))
  (declare (ignore slot-names))
  (when (or owner-supplied-p name-supplied-p)
    (let ((previous-owner (defsys:owner definition))
          (previous-name (defsys:name definition)))
      (let ((new-owner (if owner-supplied-p owner previous-owner))
            (new-name (if name-supplied-p name previous-name)))
        (if new-owner
            (setf (defsys:locate new-owner new-name 'defsys:bind-definition '(:binding-type :primary))
                  definition)
            (when previous-owner
              (defsys:unbind previous-owner previous-name)))))))

(defmethod (setf defsys:owner) (new-owner (definition defsys:primary-binding-mixin))
  (reinitialize-instance definition :owner new-owner))

(defmethod (setf defsys:name) (new-name (definition defsys:primary-binding-mixin))
  (reinitialize-instance definition :name new-name))

(defclass defsys:alias-bindings-mixin (defsys:definition)
  ((%aliasing-systems :reader %aliasing-systems
                      :type hash-table
                      :initform (make-hash-table :test 'eq))))

(defgeneric defsys:map-aliasing-systems (function definition)
  (:method (function (definition defsys:alias-bindings-mixin))
    (maphash (lambda (system aliases)
               (funcall function system (lambda (function)
                                          (mapcar function aliases))))
             (%aliasing-systems definition)))
  (:argument-precedence-order definition function))

(defclass defsys:standard-definition (defsys:primary-binding-mixin defsys:alias-bindings-mixin defsys:definition)
  ())
