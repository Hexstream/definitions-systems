(in-package #:definitions-systems)

(defclass defsys:system () ())

(defgeneric defsys:locate (system definition-name &key errorp &allow-other-keys)
  (:method :around ((system-name symbol) name &rest keys)
    (if (ikeywords:ikeywordp system-name)
        (apply #'defsys:locate
               (intern (symbol-name system-name) #.(find-package '#:keyword))
               keys)
        (call-next-method)))
  (:method ((system-name symbol) name &rest keys)
    (declare (ignore keys))
    (defsys:locate (defsys:locate 'defsys:system system-name)
                   name)))

(defgeneric (setf defsys:locate)
    (new-definition system definition-name &key errorp))


(defgeneric defsys:locator (object))

(defclass defsys:hash-table-mixin ()
  ((%hash :reader defsys:locator
          :type hash-table)))

(defun %hash-table-with-test-and-contents (test old-hash-table)
  (let ((new (make-hash-table :test test
                              :size (hash-table-size old-hash-table))))
    (prog1 new
      (maphash (lambda (key value)
                 (setf (gethash key new) value))
               old-hash-table))))

(defmethod shared-initialize :before ((mixin defsys:hash-table-mixin)
                                      slot-names
                                      &key
                                      ((#:aux boundp)
                                       (slot-boundp mixin '%hash))
                                      ((#:aux current-test)
                                       (if boundp
                                           (hash-table-test
                                            (slot-value mixin '%hash))
                                           'eq))
                                      ((:hash-table-test new-test)
                                       (if boundp
                                           current-test
                                           'eq)))
  (when (and (not boundp)
             (or (eq slot-names t) (member '%hash slot-names)))
    (setf (slot-value mixin '%hash)
          (make-hash-table :test new-test)))
  (unless (eq current-test new-test)
    (setf (slot-value mixin '%hash)
          (%hash-table-with-test-and-contents new-test (slot-value mixin '%hash)))))


(defgeneric defsys:name (system))

(defclass defsys:name-mixin ()
  ((%name :initarg :name
          :reader defsys:name
          :type symbol)))

(defmethod print-object ((mixin name-mixin) stream)
  (print-unreadable-object (mixin stream :type t)
    (prin1 (defsys:name mixin) stream)))


(defgeneric defsys:ensure (system definition-name &rest [re]definition-initargs
                                  &key &allow-other-keys))

(defmacro defsys:define (name &body options)
  (declare (ignore name options)))


(define-condition defsys:not-found (error)
  ((%system :initarg :system
            :reader defsys:system)
   (%name :initarg :name
          :reader defsys:name))
  (:report (lambda (condition stream)
             (format stream "No definition named ~S in system ~S."
                     (defsys:name condition) (defsys:system condition)))))
