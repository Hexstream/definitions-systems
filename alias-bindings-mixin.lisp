(in-package #:definitions-systems)

(defclass defsys:alias-bindings-mixin (defsys:definition)
  ((%aliasing-systems :reader %aliasing-systems
                      :type hash-table
                      :initform (make-hash-table :test 'eq))))

(defgeneric defsys:map-aliasing-systems (function definition)
  (:method (function (definition defsys:alias-bindings-mixin))
    (maphash (lambda (system aliases)
               (funcall function system (lambda (function)
                                          (mapcar function aliases))))
             (%aliasing-systems definition)))
  (:argument-precedence-order definition function))


(defmethod defsys:bind-definition :after ((system defsys:system) (new-definition defsys:alias-bindings-mixin) definition-name
                                          &key (binding-type :auto))
  (when (or (and (eq binding-type :auto) (defsys:owner new-definition))
            (eq binding-type :alias))
    (pushnew definition-name (gethash system (%aliasing-systems new-definition)) :test #'eq)))


(defmethod defsys:unbind-definition :after ((system defsys:system) (definition defsys:alias-bindings-mixin) definition-name &key)
  (let* ((aliasing-systems (%aliasing-systems definition))
         (aliases (gethash system aliasing-systems)))
    (when (member definition-name aliases :test #'eq)
      (setf (gethash system aliasing-systems) (remove definition-name aliases :test #'eq)))))
