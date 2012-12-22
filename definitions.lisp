(in-package #:definitions-systems)

(defsys:define defsys:system
  (:frontend (function defsys:systems)))

(defsys:define package
  (:class defsys:standard-proxy)
  (:backend (:locate #'find-package)))

(defsys:define class
  (:class defsys:standard-proxy)
  (:backend (:locate
             (lambda (class-name &key environment)
               (find-class class-name nil environment)))
            ((setf :locate)
             (lambda (new-class class-name &key environment)
               (setf (find-class class-name nil environment) new-class)))))

(defsys:define i:macro
  (:backend (:locate
             (lambda (name &key environment)
               (macro-function name environment)))
            ((setf :locate)
             (lambda (new-function name &key environment)
               (setf (macro-function name env))))))
