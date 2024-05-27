(in-package #:definitions-systems)

(defclass defsys:definition-order-mapping-mixin (defsys:system)
  ((%definition-ordered-list :reader %definition-ordered-list
                             :type %insertion-ordered-list
                             :initform (make-instance '%insertion-ordered-list
                                                      :hash-table (make-hash-table :test 'eq)))))

(defmethod defsys:bind-definition :after (new-definition (system defsys:definition-order-mapping-mixin) definition-name &key)
  (%insert (%definition-ordered-list system)
           definition-name
           (cons definition-name new-definition)))

(defmethod defsys:unbind-definition :after ((system defsys:definition-order-mapping-mixin) definition definition-name &key)
  (%remove (%definition-ordered-list system) definition-name))


(defmethod defsys:map (function (system defsys:definition-order-mapping-mixin) &key)
  (%map (lambda (entry)
          (funcall function (car entry) (cdr entry)))
        (%definition-ordered-list system)))
