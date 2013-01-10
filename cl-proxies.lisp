(in-package #:definitions-systems-cl)

(define defsys:system
  (:frontend (function defsys:systems)))

(define (defsys:system package
          (:class defsys-proxy:standard-proxy))
  (:backend (:locate #'find-package)))

(define (defsys:system class
          (:class defsys-proxy:standard-proxy))
  (:backend (:locate
             (lambda (class-name &key environment)
               (find-class class-name nil environment)))
            ((setf :locate)
             (lambda (new-class class-name &key environment)
               (setf (find-class class-name nil environment) new-class)))))

(define (defsys:system i:macro
          (:class defsys-proxy:standard-proxy))
  (:backend (:locate
             (lambda (name &key environment)
               (macro-function name environment)))
            ((setf :locate)
             (lambda (new-function name &key environment)
               (setf (macro-function name env))))))
