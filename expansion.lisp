(in-package #:definitions-systems)

;;; Backend

(defmethod defsys:ensure ((system defsys:system) definition-name definition-class &rest initargs)
  (let ((existing (defsys:locate system definition-name :errorp nil)))
    (if existing
        (let ((target-class (find-class definition-class)))
          (if (eq (class-of existing) target-class)
              (apply #'reinitialize-instance existing initargs)
              (apply #'change-class existing target-class initargs)))
        (setf (defsys:locate system definition-name)
              (apply #'make-instance definition-class :name definition-name initargs)))))

(defmethod defsys:default-system ((definition-class-name symbol))
  (defsys:default-system (find-class definition-class-name)))

(defmethod defsys:default-system ((definition-class class))
  (defsys:default-system (c2mop:class-prototype (c2mop:ensure-finalized definition-class))))


(defmethod defsys:expand ((prototype defsys:standard-definition) name env args &rest options)
  (declare (ignore env options))
  `(defsys:ensure ,(defsys:default-system prototype)
                  ',name
                  ,(class-of prototype)
                  ,@args))

(defmethod defsys:expand ((prototype defsys:standard-system) name env args &rest options)
  (declare (ignore env args options))
  `(progn
     (cl:defclass ,name (defsys:definition) ())
     ,(append (call-next-method) `(:base-definition-class ',name))
     (defmethod defsys:default-system ((definition ,name))
       (load-time-value (defsys:locate 'defsys:system ',name)))))


;;; Frontend

(defmacro defsys:define ((definition-class-name definition-name &body options)
                         &body args &environment env)
  (apply #'defsys:expand (c2mop:class-prototype (c2mop:ensure-finalized (find-class definition-class-name)))
         definition-name env args options))
