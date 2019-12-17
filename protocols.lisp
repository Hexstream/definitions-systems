(in-package #:definitions-systems)

(defgeneric defsys:locate (system definition-name &key errorp)
  (:method :around (system definition-name &key (errorp t))
    (or (call-next-method)
        (when errorp
          (error 'defsys:not-found :system system :name definition-name))))
  (:method ((system-name symbol) definition-name &key (errorp t))
    (defsys:locate (defsys:locate (defsys:root-system) system-name)
                   definition-name :errorp errorp))
  (:method ((system defsys:hash-table-mixin) definition-name &key (errorp t))
    (declare (ignore errorp))
    (identity (gethash definition-name (slot-value system '%hash)))))

(define-condition defsys:not-found (error)
  ((%system :initarg :system
            :reader defsys:system
            :type defsys:system)
   (%name :initarg :name
          :reader defsys:name))
  (:report (lambda (condition stream)
             (format stream "No definition named ~S in system ~S."
                     (defsys:name condition) (defsys:system condition)))))

(defgeneric (setf defsys:locate) (new-definition system definition-name &key errorp)
  (:method (new-definition (system-name symbol) definition-name &key (errorp nil))
    (setf (defsys:locate (defsys:locate (defsys:root-system) system-name)
                         definition-name
                         :errorp errorp)
          new-definition))
  (:method :before (new-definition (system defsys:check-definition-mixin) definition-name &key errorp)
    (declare (ignore errorp))
    (check-definition system new-definition))
  (:method :after ((new-definition defsys:owner-mixin) (system defsys:system) definition-name &key errorp)
    (declare (ignore errorp))
    (unless (defsys:owner new-definition)
      (setf (slot-value new-definition '%owner) system)))
  (:method (new-definition (system defsys:hash-table-mixin) definition-name &key errorp)
    (declare (ignore errorp))
    (setf (gethash definition-name (slot-value system '%hash))
          new-definition)))

(defgeneric defsys:unbind (system definition-name)
  (:method ((system-name symbol) definition-name)
    (defsys:unbind (defsys:locate (defsys:root-system) system-name)
                   definition-name))
  (:method ((system defsys:system) definition-name)
    (let ((definition (defsys:locate system definition-name :errorp nil)))
      (when definition
        (defsys:unbind-definition system definition definition-name)))))

(defgeneric defsys:unbind-definition (system definition definition-name)
  (:method ((system defsys:hash-table-mixin) definition definition-name)
    (declare (ignore definition))
    (remhash definition-name (slot-value system '%hash)))
  (:method :after ((system defsys:system) (definition defsys:owner-mixin) definition-name)
    (declare (ignore definition-name))
    (let ((owner (defsys:owner definition)))
      (when (eq owner system)
        (setf (slot-value definition '%owner) nil)))))

(defgeneric defsys:boundp (system definition-name)
  (:method (system definition-name)
    (not (null (defsys:locate system definition-name :errorp nil)))))


(defgeneric defsys:ensure (system definition-name class &rest initargs)
  (:method ((system-name symbol) definition-name class &rest initargs)
    (apply #'defsys:ensure
           (defsys:locate (defsys:root-system) system-name)
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


(defgeneric defsys:expand (system definition-name environment args &rest options)
  (:method ((system-name symbol) definition-name environment args &rest options)
    (apply #'defsys:expand
           (defsys:locate (defsys:root-system) system-name)
           definition-name environment args options)))

(defmacro defsys:define ((kind definition-name &body options)
                         &body args &environment env)
  (apply #'defsys:expand kind definition-name env args options))


(defgeneric defsys:map (function system)
  (:method (function (system-name symbol))
    (defsys:map function (defsys:locate (defsys:root-system) system-name)))
  (:method (function (system defsys:hash-table-mixin))
    (maphash function (slot-value system '%hash))))
