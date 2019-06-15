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


(defclass defsys:standard-system (defsys:name-mixin defsys:hash-table-mixin)
  ())

(defvar *root-system* (make-instance 'defsys:standard-system :name 'defsys:system))

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
    (identity (gethash definition-name (slot-value system '%hash)))))

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
           definition-name environment args options)))

(defmacro defsys:define ((kind definition-name &body options)
                         &body args &environment env)
  (apply #'defsys:expand-definition kind definition-name env args options))


(define-condition defsys:not-found (error)
  ((%system :initarg :system
            :reader defsys:system)
   (%name :initarg :name
          :reader defsys:name))
  (:report (lambda (condition stream)
             (format stream "No definition named ~S in system ~S."
                     (defsys:name condition) (defsys:system condition)))))
