(in-package #:definitions-systems)

(defclass defsys:check-definition-mixin (defsys:system) ())

(defgeneric defsys:check-definition (system definition)
  (:method ((system defsys:system) definition)
    definition))


(defgeneric defsys:base-definition-class (system))

(defclass defsys:base-definition-class-mixin (defsys:check-definition-mixin)
  ((%base-definition-class :initarg :base-definition-class
                           :reader defsys:base-definition-class
                           :type class
                           :canonicalize #'find-class
                           :initform (find-class 'defsys:definition))))

(define-condition defsys:unsuitable-definition-error (error)
  ((%system :initarg :system
            :reader defsys:system
            :type defsys:system
            :initform (error "~S is required." :system))
   (%definition :initarg :definition
                :reader defsys:definition
                :initform (error "~S is required." :definition))
   (%details :initarg :details
             :reader defsys:details
             :initform nil))
  (:report (lambda (error stream)
             (format stream "~S is not a suitable definition for system ~S ~@
                             Details: ~S"
                     (defsys:definition error) (defsys:system error) (defsys:details error)))))

(defmethod defsys:check-definition ((system defsys:base-definition-class-mixin) definition)
  (let ((base-definition-class (defsys:base-definition-class system)))
    (if (typep definition base-definition-class)
        definition
        (error 'defsys:unsuitable-definition-error
               :system system :definition definition
               :details (make-condition 'type-error
                                        :datum definition
                                        :expected-type base-definition-class)))))


(defmethod defsys:bind-definition :before ((system defsys:check-definition-mixin) new-definition definition-name &key)
  (declare (ignore definition-name))
  (check-definition system new-definition))
