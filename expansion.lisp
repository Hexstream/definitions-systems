(in-package #:definitions-systems)

(defun %maybe-find-class (class-designator)
  (when class-designator
    (find-class class-designator)))

(defclass defsys:default-definition-class-mixin (defsys:system)
  ((%default-definition-class :initarg :default-definition-class
                              :type (or null class)
                              :canonicalize #'%maybe-find-class
                              :initform nil)))

(defgeneric defsys:default-definition-class (system &key)
  (:method :around (system &key (errorp t))
    (or (call-next-method)
        (when errorp
          (error "There is no ~S for system ~S." 'defsys:default-definition-class system))))
  (:method ((system defsys:default-definition-class-mixin) &key)
    (slot-value system '%default-definition-class)))


(defclass defsys:simple-expansion-mixin (defsys:default-definition-class-mixin)
  ((%explicit-definition-class-p :initarg :explicit-definition-class-p
                                 :reader defsys:explicit-definition-class-p
                                 :type boolean
                                 :initform nil)))

(defgeneric defsys:definition-class (system &rest initargs)
  (:method ((system defsys:system) &rest initargs)
    (declare (ignore initargs))
    (defsys:default-definition-class system)))

(defgeneric defsys:expand-args (system &rest initargs)
  (:method ((system-name symbol) &rest initargs)
    (apply #'defsys:expand-args
           (defsys:locate (defsys:root-system) system-name)
           initargs))
  (:method ((system defsys:system) &rest initargs)
    initargs))

(defun %every-other (function)
  (let ((processp t))
    (lambda (key value)
      (prog1 (when processp
               (funcall function key value))
        (setf processp (not processp))))))

(defun %mappc (function plist)
  (mapc (%every-other function) plist (cdr plist)))

(defun %mappcon (function plist)
  (mapcan (%every-other function) plist (cdr plist)))

(defun %fix-args (args)
  (if (block nil
        (%mappc (lambda (key value)
                  (declare (ignore value))
                  (unless (keywordp key)
                    (return t)))
                args))
      (%mappcon (lambda (key value)
                  (list (if (keywordp key)
                            key
                            `',key)
                        value))
                args)
      args))

(defun %canonicalize-definition-class (maybe-explicit-definition-class implicit-definition-class)
  (let ((maybe-explicit-definition-class-var (gensym (string '#:maybe-explicit-definition-class))))
    `(let ((,maybe-explicit-definition-class-var ,maybe-explicit-definition-class))
       (if (eq ,maybe-explicit-definition-class-var t)
           ,implicit-definition-class
           ,maybe-explicit-definition-class-var))))

(defmethod defsys:expand ((system defsys:simple-expansion-mixin) name environment args &key)
  (declare (ignore environment))
  (let ((initargs-var (gensym (string '#:initargs))))
    (multiple-value-bind (definition-class-form args)
        (let ((implicit-definition-class-form
               `(apply #'defsys:definition-class ,system ',name ,initargs-var)))
          (if (defsys:explicit-definition-class-p system)
              (destructuring-bind (maybe-explicit-definition-class &rest args) args
                (values (%canonicalize-definition-class maybe-explicit-definition-class
                                                        implicit-definition-class-form)
                        args))
              (values implicit-definition-class-form args)))
      `(let ((,initargs-var (list ,@(%fix-args (apply #'defsys:expand-args system args)))))
         (apply #'defsys:ensure ,system ',name ,definition-class-form ,initargs-var)))))
