(in-package #:definitions-systems)

(defclass defsys:root-system (defsys:system) ())

(defclass defsys:standard-root-system (defsys:simple-expansion-mixin defsys:root-system defsys:standard-system)
  ()
  (:default-initargs :explicit-definition-class-p t))

(defvar *root-system* (make-instance 'defsys:standard-root-system :name 'defsys:system))

(defun defsys:root-system ()
  *root-system*)

(defmethod make-load-form ((root-system defsys:standard-root-system) &optional environment)
  (declare (ignore environment))
  '(defsys:root-system))

(defmethod defsys:locate ((system defsys:standard-root-system) definition-name &key (errorp t))
  (declare (ignore errorp))
  (if (eq definition-name 'defsys:system)
      system
      (call-next-method)))
