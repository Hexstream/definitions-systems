(in-package #:definitions-systems)

;; See systems.lisp for (defclass defsys:hash-table-mixin).

(defmethod defsys:locate ((system defsys:hash-table-mixin) definition-name &key)
  (identity (gethash definition-name (%hash system))))

(defmethod defsys:bind-definition ((system defsys:hash-table-mixin) new-definition definition-name &key)
  (setf (gethash definition-name (%hash system))
        new-definition))

(defmethod defsys:unbind-definition ((system defsys:hash-table-mixin) definition definition-name &key)
  (declare (ignore definition))
  (remhash definition-name (%hash system)))

(defmethod defsys:map (function (system defsys:hash-table-mixin) &key)
  (maphash function (%hash system)))

(defmethod defsys:count ((system defsys:hash-table-mixin) &key)
  (hash-table-count (%hash system)))
