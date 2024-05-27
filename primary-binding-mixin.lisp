(in-package #:definitions-systems)

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


(defclass defsys:name-mixin ()
  ((%name :reader defsys:name
          :writer (setf %name)
          :type symbol
          :initform nil)))

(defmethod print-object ((mixin name-mixin) stream)
  (print-unreadable-object (mixin stream :type t)
    (prin1 (defsys:name mixin) stream)))


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


(defmethod defsys:bind-definition :after ((system defsys:system) (new-definition defsys:primary-binding-mixin) definition-name
                                          &key (binding-type :auto))
  (let ((previous-owner (defsys:owner new-definition)))
    (when (or (and (eq binding-type :auto) (not previous-owner))
              (eq binding-type :primary))
      (when previous-owner
        (defsys:unbind previous-owner (defsys:name new-definition)))
      (setf (%owner new-definition) system
            (%name new-definition) definition-name))))

(defmethod defsys:unbind-definition :after ((system defsys:system) (definition defsys:primary-binding-mixin) definition-name &key)
  (declare (ignore definition-name))
  (let ((owner (defsys:owner definition)))
    (when (eq owner system)
      (setf (%owner definition) nil))))
