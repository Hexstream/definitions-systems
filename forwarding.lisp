(in-package #:definitions-systems)

(defmethod defsys:locate ((system-name symbol) definition-name &rest options &key &allow-other-keys)
  (apply #'defsys:locate (defsys:locate (defsys:root-system) system-name)
         definition-name options))

(defmethod (setf defsys:locate) (new-definition (system-name symbol) definition-name &rest options &key &allow-other-keys)
  (setf (apply #'defsys:locate (defsys:locate (defsys:root-system) system-name)
               definition-name
               options)
        new-definition))

(defmethod defsys:unbind ((system-name symbol) definition-name &rest options &key &allow-other-keys)
  (apply #'defsys:unbind
           (defsys:locate (defsys:root-system) system-name)
           definition-name
           options))

(defmethod defsys:ensure ((system-name symbol) definition-name definition-class &rest initargs)
  (apply #'defsys:ensure
         (defsys:locate (defsys:root-system) system-name)
         definition-name definition-class initargs))

(defmethod defsys:map (function (system-name symbol) &rest options &key &allow-other-keys)
  (apply #'defsys:map function (defsys:locate (defsys:root-system) system-name)
           options))

(defmethod defsys:count ((system-name symbol) &rest options &key &allow-other-keys)
  (apply #'defsys:count (defsys:locate (defsys:root-system) system-name)
         options))
