(cl:defpackage #:definitions-systems_tests
  (:use #:cl #:parachute)
  (:import-from #:defsys #:locate))

(cl:in-package #:definitions-systems_tests)

(defmacro are (comp expected form &optional description &rest format-args)
  `(is ,comp ,expected (multiple-value-list ,form) ,description ,@format-args))

(defclass base-definition-class-system (defsys:base-definition-class-mixin
                                        defsys:hash-table-mixin)
  ())

(define-test "main"
  (flet ((basic-tests (system)
           (flet ((check-unbound ()
                    (false (locate system 'key :errorp nil))
                    (fail (locate system 'key))
                    (false (defsys:boundp system 'key))
                    (is = 0
                        (defsys:count system))))
             (check-unbound)
             (setf (locate system 'key) 'value)
             (is eq 'value
                 (locate system 'key))
             (defsys:map (let ((count 0))
                           (lambda (key value)
                             (incf count)
                             (are equal '(1 key value)
                                  (values count key value))))
                         system)
             (defsys:unbind system 'key)
             (check-unbound))))
    (basic-tests (make-instance 'defsys:hash-table-mixin))
    (basic-tests (make-instance 'defsys:standard-system :base-definition-class t)))
  (fail (setf (locate (make-instance 'base-definition-class-system) 'test)
              'wrong-type)
        'defsys:unsuitable-definition-error)
  (let ((system (make-instance 'defsys:standard-system))
        (definition (make-instance 'defsys:primary-binding-mixin)))
    (setf (locate system 'name-a) definition)
    (is eq system (defsys:owner definition))
    (is eq 'name-a (defsys:name definition))
    (setf (defsys:name definition) 'name-b)
    (is eq 'name-b (defsys:name definition))
    (false (defsys:boundp system 'name-a))
    (is eq definition (locate system 'name-b))
    (let ((system-b (make-instance 'defsys:standard-system)))
      (setf (defsys:owner definition) system-b)
      (false (defsys:boundp system 'name-b))
      (is eq definition (locate system-b 'name-b))
      (reinitialize-instance definition :owner system :name 'name-c)
      (false (defsys:boundp system-b 'name-b))
      (is eq definition (locate system 'name-c))
      (setf (locate system-b 'name-d :binding-type :primary) definition)
      (false (defsys:boundp system 'name-c))
      (is eq definition (locate system-b 'name-d))
      (defsys:unbind system-b 'name-d)
      (false (defsys:boundp system-b 'name-d))
      (false (defsys:owner definition))
      (eq 'name-d (defsys:name definition)))))


; primary-binding-mixin
; alias-bindings-mixin
; map-aliasing-systems
; ensure, expand, define, map, count
; simple-expansion-mixin
