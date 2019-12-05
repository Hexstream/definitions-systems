(in-package #:definitions-systems)

(defclass defsys:simple-expansion-mixin ()
  ((%explicit-definition-class-p :initarg :explicit-definition-class-p
                                 :reader defsys:explicit-definition-class-p
                                 :type boolean
                                 :initform nil)
   (%default-definition-class :initarg :default-definition-class
                              :reader defsys:default-definition-class
                              :type class)))

(defun %canonicalize-definition-class (maybe-explicit-definition-class implicit-definition-class)
  (if (eq maybe-explicit-definition-class t)
      implicit-definition-class
      maybe-explicit-definition-class))

(defmethod defsys:expand-definition ((system defsys:simple-expansion-mixin) name environment args &key)
  (declare (ignore environment))
  (let ((initargs-var (gensym (string '#:initargs))))
    (multiple-value-bind (definition-class-form args)
        (let ((implicit-definition-class-form
               `(apply #'definition-class ,system ',name ,initargs-var)))
          (if (defsys:explicit-definition-class-p system)
              (destructuring-bind (maybe-explicit-definition-class &rest args) args
                (values `(%canonicalize-definition-class ,maybe-explicit-definition-class
                                                         ,implicit-definition-class-form)
                        args))
              (values implicit-definition-class-form args)))
      `(let ((,initargs-var (list ,@(apply #'expand-definition-args system name args))))
         (apply #'defsys:ensure ,system ',name ,definition-class-form ,initargs-var)))))

(defgeneric expand-definition-args (system definition-name &rest initargs)
  (:method ((system-name symbol) definition-name &rest initargs)
    (apply #'expand-definition-args
           (defsys:locate (defsys:root-system) system-name)
           initargs))
  (:method ((system defsys:system) definition-name &rest initargs)
    initargs))

(defgeneric definition-class (system definition-name &rest initargs)
  (:method ((system defsys:system) name &rest initargs)
    (declare (ignore name initargs))
    (defsys:default-definition-class system)))
