(in-package #:definitions-systems)

(defclass defsys:system () ())


(defgeneric defsys:name (system))

(defclass defsys:name-mixin ()
  ((%name :initarg :name
          :reader defsys:name
          :type symbol)))

(defmethod print-object ((mixin name-mixin) stream)
  (print-unreadable-object (mixin stream :type t)
    (prin1 (defsys:name mixin) stream)))


(defclass defsys:hash-table-mixin ()
  ((%hash :type hash-table :initform (make-hash-table :test 'eq))))


(defclass defsys:standard-system (defsys:name-mixin defsys:hash-table-mixin defsys:system)
  ())

(defclass %root-system (defsys:standard-system) ())

(defvar *root-system* (make-instance '%root-system :name 'defsys:system))

(defun defsys:root-system ()
  *root-system*)


(defgeneric defsys:locate (system definition-name &key errorp)
  (:method :around (system definition-name &key (errorp t))
    (or (call-next-method)
        (when errorp
          (error 'defsys:not-found :system system :name definition-name))))
  (:method ((system-name symbol) definition-name &key (errorp t))
    (defsys:locate (defsys:locate *root-system* system-name)
                   definition-name :errorp errorp))
  (:method ((system defsys:hash-table-mixin) definition-name &key (errorp t))
    (declare (ignore errorp))
    (identity (gethash definition-name (slot-value system '%hash))))
  (:method ((system %root-system) definition-name &key (errorp t))
    (declare (ignore errorp))
    (if (eq definition-name 'defsys:system)
        system
        (call-next-method))))

(defgeneric (setf defsys:locate) (new-definition system definition-name &key errorp)
  (:method (new-definition (system-name symbol) definition-name &key (errorp nil))
    (setf (defsys:locate (defsys:locate *root-system* system-name)
                         definition-name
                         :errorp errorp)
          new-definition))
  (:method (new-definition (system defsys:hash-table-mixin) definition-name &key (errorp nil))
    (declare (ignore errorp))
    (setf (gethash definition-name (slot-value system '%hash))
          new-definition)))

(defgeneric defsys:unbind (system definition-name)
  (:method ((system-name symbol) definition-name)
    (defsys:unbind (defsys:locate *root-system* system-name)
                   definition-name))
  (:method ((system hash-table-mixin) definition-name)
    (remhash definition-name (slot-value system '%hash))))

(defgeneric defsys:boundp (system definition-name)
  (:method (system definition-name)
    (not (null (defsys:locate system definition-name :errorp nil)))))


(defgeneric defsys:expand-definition (system definition-name environment args &rest options)
  (:method ((system-name symbol) definition-name environment args &rest options)
    (apply #'defsys:expand-definition
           (defsys:locate *root-system* system-name)
           definition-name environment args options))
  (:method ((system %root-system) name environment args &key)
    (declare (ignore environment))
    (destructuring-bind (class &rest args) args
      `(defsys:ensure ',(defsys:name system) ',name ',class ,@args))))

(defmacro defsys:define ((kind definition-name &body options)
                         &body args &environment env)
  (apply #'defsys:expand-definition kind definition-name env args options))



(defgeneric defsys:ensure (system definition-name class &rest initargs)
  (:method ((system-name symbol) definition-name class &rest initargs)
    (apply #'defsys:ensure
           (defsys:locate *root-system* system-name)
           definition-name class initargs))
  (:method ((system defsys:system) definition-name definition-class &rest initargs)
    (let ((existing (defsys:locate system definition-name :errorp nil)))
      (if existing
          (let ((target-class (etypecase definition-class
                                (class definition-class)
                                (symbol (find-class definition-class)))))
            (if (eq (class-of existing) target-class)
                (apply #'reinitialize-instance existing initargs)
                (apply #'change-class existing target-class initargs)))
          (setf (defsys:locate system definition-name)
                (apply #'make-instance definition-class :name definition-name initargs))))))


(define-condition defsys:not-found (error)
  ((%system :initarg :system
            :reader defsys:system)
   (%name :initarg :name
          :reader defsys:name))
  (:report (lambda (condition stream)
             (format stream "No definition named ~S in system ~S."
                     (defsys:name condition) (defsys:system condition)))))
