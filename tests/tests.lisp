(cl:defpackage #:definitions-systems_tests
  (:use #:cl #:parachute))

(cl:in-package #:definitions-systems_tests)

(defmacro are (comp expected form &optional description &rest format-args)
  `(is ,comp ,expected (multiple-value-list ,form) ,description ,@format-args))

(define-test "main"
  (flet ((basic-tests (system)
           (flet ((check-unbound ()
                    (false (defsys:locate system 'key :errorp nil))
                    (fail (defsys:locate system 'key))
                    (false (defsys:boundp system 'key))
                    (is = 0
                        (defsys:count system))))
             (check-unbound)
             (setf (defsys:locate system 'key) 'value)
             (is eq 'value
                 (defsys:locate system 'key))
             (defsys:map (let ((count 0))
                           (lambda (key value)
                             (incf count)
                             (are equal '(1 key value)
                                  (values count key value))))
                         system)
             (defsys:unbind system 'key)
             (check-unbound))))
    (basic-tests (make-instance 'defsys:hash-table-mixin))
    (basic-tests (make-instance 'defsys:standard-system :base-definition-class t))))

; base-definition-class-mixin
; primary-binding-mixin
; alias-bindings-mixin
; map-aliasing-systems
; ensure, expand, define, map, count
; simple-expansion-mixin
