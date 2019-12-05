(in-package #:definitions-systems)

(defclass defsys:simple-expansion-mixin ()
  ((%explicit-definition-class-p :initarg :explicit-definition-class-p
                                 :reader defsys:explicit-definition-class-p
                                 :type boolean
                                 :initform nil)
   (%default-definition-class :initarg :default-definition-class
                              ;:reader defsys:default-definition-class (see method below)
                              :type (or null class)
                              :initform nil)))

(defgeneric defsys:default-definition-class (system &key errorp)
  (:method :around (system &key (errorp t))
    (or (call-next-method)
        (when errorp
          (error "There is no ~S for system ~S." 'defsys:default-definition-class system))))
  (:method ((system defsys:simple-expansion-mixin) &key (errorp t))
    (declare (ignore errorp))
    (slot-value system '%default-definition-class)))

(defgeneric defsys:definition-class (system definition-name &rest initargs)
  (:method ((system defsys:system) name &rest initargs)
    (declare (ignore name initargs))
    (defsys:default-definition-class system)))

(defgeneric defsys:expand-definition-args (system definition-name &rest initargs)
  (:method ((system-name symbol) definition-name &rest initargs)
    (apply #'defsys:expand-definition-args
           (defsys:locate (defsys:root-system) system-name)
           initargs))
  (:method ((system defsys:system) definition-name &rest initargs)
    initargs))

(defun %canonicalize-definition-class (maybe-explicit-definition-class implicit-definition-class)
  (if (eq maybe-explicit-definition-class t)
      implicit-definition-class
      maybe-explicit-definition-class))

(defmethod defsys:expand-definition ((system defsys:simple-expansion-mixin) name environment args &key)
  (declare (ignore environment))
  (let ((initargs-var (gensym (string '#:initargs))))
    (multiple-value-bind (definition-class-form args)
        (let ((implicit-definition-class-form
               `(apply #'defsys:definition-class ,system ',name ,initargs-var)))
          (if (defsys:explicit-definition-class-p system)
              (destructuring-bind (maybe-explicit-definition-class &rest args) args
                (values `(%canonicalize-definition-class ,maybe-explicit-definition-class
                                                         ,implicit-definition-class-form)
                        args))
              (values implicit-definition-class-form args)))
      `(let ((,initargs-var (list ,@(apply #'defsys:expand-definition-args system name args))))
         (apply #'defsys:ensure ,system ',name ,definition-class-form ,initargs-var)))))
