(in-package #:definitions-systems)

(defclass defsys:system () ())


(defclass defsys:hash-table-mixin ()
  ((%hash :type hash-table :initform (make-hash-table :test 'eq))))

(defclass defsys:check-definition-mixin () ())

(defgeneric defsys:base-definition-class (system))

(defclass defsys:base-definition-class-mixin (defsys:check-definition-mixin)
  ((%base-definition-class :initarg :base-definition-class
                           :reader defsys:base-definition-class
                           :type class
                           :initform (find-class 'defsys:definition))))

(define-condition defsys:unsuitable-definition-error (error)
  ((%system :initarg :system
            :reader defsys:system
            :type defsys:system
            :initform (error "~S is required." :system))
   (%definition :initarg :definition
                :reader defsys:definition
                :initform (error "~S is required." :definition))
   (%name :initarg :name
          :reader defsys:name
          :initform (error "~S is required." :name))
   (%details :initarg :details
             :reader defsys:details
             :initform nil))
  (:report (lambda (error stream)
             (format stream "~S is not a suitable definition for system ~S (and name ~S). ~@
                             Details: ~S"
                     (defsys:definition error) (defsys:system error) (defsys:name error)
                     (defsys:details error)))))

(defgeneric defsys:check-definition (system definition definition-name)
  (:method ((system defsys:system) definition definition-name)
    (declare (ignore definition-name))
    definition)
  (:method ((system defsys:base-definition-class-mixin) definition definition-name)
    (let ((base-definition-class (defsys:base-definition-class system)))
      (if (typep definition base-definition-class)
          definition
          (error 'defsys:unsuitable-definition-error
                 :system system :definition definition :name definition-name
                 :details (make-condition 'type-error
                                          :datum definition
                                          :expected-type base-definition-class))))))


(defclass defsys:standard-system (defsys:system
                                  defsys:base-definition-class-mixin
                                  defsys:standard-definition
                                  defsys:hash-table-mixin)
  ())

(defclass defsys:root-system (defsys:system) ())

(defclass defsys:standard-root-system (defsys:root-system defsys:standard-system) ())

(defvar *root-system* (make-instance 'defsys:standard-root-system :name 'defsys:system))

(defun defsys:root-system ()
  *root-system*)

(defmethod make-load-form ((root-system defsys:standard-root-system) &optional environment)
  (declare (ignore environment))
  '(defsys:root-system))


(define-condition defsys:not-found (error)
  ((%system :initarg :system
            :reader defsys:system
            :type defsys:system)
   (%name :initarg :name
          :reader defsys:name))
  (:report (lambda (condition stream)
             (format stream "No definition named ~S in system ~S."
                     (defsys:name condition) (defsys:system condition)))))
