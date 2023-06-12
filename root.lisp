(in-package #:definitions-systems)

(defclass defsys:root-system (defsys:system) ())

(defclass defsys:location-mixin ()
  ((%location :initarg :location
              :initform nil)))

(defgeneric defsys:location (system &key)
  (:method :around (system &key (errorp t))
    (or (call-next-method)
        (when errorp
          (error "There is no ~S for system ~S." 'defsys:location system))))
  (:method ((system defsys:location-mixin) &key)
    (slot-value system '%location)))

(defmethod make-load-form ((mixin defsys:location-mixin) &optional environment)
  (declare (ignore environment))
  (or (defsys:location mixin :errorp nil)
      (call-next-method)))

(defclass defsys:standard-root-system (defsys:location-mixin defsys:root-system defsys:standard-system) ())

(defmethod defsys:locate ((system defsys:standard-root-system) definition-name &key (errorp t))
  (declare (ignore errorp))
  (if (eq definition-name 'defsys:system)
      system
      (call-next-method)))

(defvar *root-system* (make-instance 'defsys:standard-root-system :name 'defsys:system :location '(defsys:root-system)))

(defun defsys:root-system ()
  *root-system*)
