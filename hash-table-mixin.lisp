(in-package #:definitions-systems)

;; See systems.lisp for (defclass defsys:hash-table-mixin).

(defmethod defsys:locate ((system defsys:hash-table-mixin) definition-name &key)
  (identity (gethash definition-name (%hash system))))

(defmethod (setf defsys:locate) (new-definition (system defsys:hash-table-mixin) definition-name &key)
  (setf (gethash definition-name (%hash system))
        new-definition))

(defmethod defsys:unbind-definition ((system defsys:hash-table-mixin) definition definition-name)
  (declare (ignore definition))
  (remhash definition-name (%hash system)))

(defmethod defsys:map (function (system defsys:hash-table-mixin))
  (maphash function (%hash system)))

(defmethod defsys:count ((system defsys:hash-table-mixin))
  (hash-table-count (%hash system)))
