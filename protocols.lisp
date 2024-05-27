(in-package #:definitions-systems)

(defgeneric defsys:locate (system definition-name &key)
  (:method :around (system definition-name &key (errorp t))
    (or (call-next-method)
        (when errorp
          (error 'defsys:not-found :system system :name definition-name))))
  (:method ((system defsys:standard-root-system) definition-name &key)
    (if (eq definition-name 'defsys:system)
        system
        (call-next-method))))

(define-condition defsys:not-found (error)
  ((%system :initarg :system
            :reader defsys:system
            :type defsys:system)
   (%name :initarg :name
          :reader defsys:name))
  (:report (lambda (condition stream)
             (format stream "No definition named ~S in system ~S."
                     (defsys:name condition) (defsys:system condition)))))

(defgeneric (setf defsys:locate) (new-definition system definition-name &key)
  (:method :before (new-definition (system defsys:check-definition-mixin) definition-name &key)
    (declare (ignore definition-name))
    (check-definition system new-definition))
  (:method :after ((new-definition defsys:primary-binding-mixin) (system defsys:system) definition-name
                   &key (binding-type :auto))
    (let ((previous-owner (defsys:owner new-definition)))
      (when (or (and (eq binding-type :auto) (not previous-owner))
                (eq binding-type :primary))
        (when previous-owner
          (defsys:unbind previous-owner (defsys:name new-definition)))
        (setf (%owner new-definition) system
              (%name new-definition) definition-name))))
  (:method :after ((new-definition defsys:alias-bindings-mixin) (system defsys:system) definition-name
                   &key (binding-type :auto))
    (when (or (and (eq binding-type :auto) (defsys:owner new-definition))
              (eq binding-type :alias))
      (pushnew definition-name (gethash system (%aliasing-systems new-definition)) :test #'eq)))
  (:method :after (new-definition (system defsys:definition-order-mapping-mixin) definition-name &key)
    (%insert (%definition-ordered-list system)
             definition-name
             (cons definition-name new-definition))))

(defgeneric defsys:unbind (system definition-name)
  (:method ((system defsys:system) definition-name)
    (let ((definition (defsys:locate system definition-name :errorp nil)))
      (when definition
        (defsys:unbind-definition system definition definition-name)))))

(defgeneric defsys:unbind-definition (system definition definition-name)
  (:method :after ((system defsys:definition-order-mapping-mixin) definition definition-name)
    (%remove (%definition-ordered-list system) definition-name))
  (:method :after ((system defsys:system) (definition defsys:primary-binding-mixin) definition-name)
    (declare (ignore definition-name))
    (let ((owner (defsys:owner definition)))
      (when (eq owner system)
        (setf (%owner definition) nil))))
  (:method :after ((system defsys:system) (definition defsys:alias-bindings-mixin) definition-name)
    (let* ((aliasing-systems (%aliasing-systems definition))
           (aliases (gethash system aliasing-systems)))
      (when (member definition-name aliases :test #'eq)
        (setf (gethash system aliasing-systems) (remove definition-name aliases :test #'eq))))))


(defgeneric defsys:ensure (system definition-name definition-class &rest initargs)
  (:method ((system defsys:system) definition-name definition-class &rest initargs)
    (let ((existing (defsys:locate system definition-name :errorp nil)))
      (if existing
          (let ((target-class (find-class definition-class)))
            (if (eq (class-of existing) target-class)
                (apply #'reinitialize-instance existing initargs)
                (apply #'change-class existing target-class initargs)))
          (setf (defsys:locate system definition-name)
                (apply #'make-instance definition-class :name definition-name initargs))))))

(defgeneric defsys:default-system (object)
  (:method ((definition-class-name symbol))
    (defsys:default-system (find-class definition-class-name)))
  (:method ((definition-class class))
    (defsys:default-system (c2mop:class-prototype (c2mop:ensure-finalized definition-class))))
  (:method ((system defsys:standard-system))
    (defsys:root-system))
  (:method ((system defsys:standard-root-system))
    (error "~S does not define a ~S." 'defsys:standard-root-system 'defsys:default-system)))

(defgeneric defsys:expand (definition-prototype definition-name environment args &rest options)
  (:method ((prototype defsys:standard-definition) name env args &rest options)
    (declare (ignore env options))
    `(defsys:ensure ,(defsys:default-system prototype)
                    ',name
                    ,(class-of prototype)
                    ,@args))
  (:method ((prototype defsys:standard-system) name env args &rest options)
    (declare (ignore env args options))
    `(progn
       (cl:defclass ,name (defsys:definition) ())
       ,(append (call-next-method) `(:base-definition-class ',name))
       (defmethod defsys:default-system ((definition ,name))
         (load-time-value (defsys:locate 'defsys:system ',name))))))

(defmacro defsys:define ((definition-class-name definition-name &body options)
                         &body args &environment env)
  (apply #'defsys:expand (c2mop:class-prototype (c2mop:ensure-finalized (find-class definition-class-name)))
         definition-name env args options))


(defgeneric defsys:map (function system)
  (:method (function (system defsys:definition-order-mapping-mixin))
    (%map (lambda (entry)
            (funcall function (car entry) (cdr entry)))
          (%definition-ordered-list system)))
  (:argument-precedence-order system function))

(defgeneric defsys:count (system)
  (:method ((system defsys:system))
    (warn "Using slow ~S." 'defsys:count)
    (let ((count 0))
      (defsys:map (lambda (name definition)
                    (declare (ignore name definition))
                    (incf count))
                  system)
      count)))
