(in-package #:definitions-systems)

;;; Frontend

(defgeneric defsys:locate (system definition-name &key))

(defgeneric (setf defsys:locate) (new-definition system definition-name &key))

(defgeneric defsys:unbind (system definition-name &key))

(defgeneric defsys:map (function system &key)
  (:argument-precedence-order system function))

(defgeneric defsys:count (system &key))


(defgeneric defsys:owner (definition))
(defgeneric (setf defsys:owner) (new-owner definition))

(defgeneric defsys:name (definition))
(defgeneric (setf defsys:name) (new-name definition))


;;; Backend

(defgeneric defsys:bind-definition (system new-definition definition-name &key))

(defgeneric defsys:unbind-definition (system definition definition-name &key))

(defgeneric defsys:replace-definition (system old-definition new-definition definition-name &key))


;;; Expansion

(defgeneric defsys:ensure (system definition-name definition-class &rest initargs &key &allow-other-keys))

(defgeneric defsys:default-system (object))

(defgeneric defsys:expand (definition-prototype definition-name environment args &rest options))
