(in-package #:definitions-systems)

(defclass defsys:system () ())

(defun %forward-ikeyword (forward continue system-name definition-name keys
                          &optional (new nil newp))
  (if (ikeywords:ikeywordp system-name)
      (multiple-value-call forward
        (if newp new (values))
        (identity (intern (symbol-name system-name)
                          #.(find-package '#:keyword)))
        definition-name
        (values-list keys))
      (funcall continue)))

(defgeneric defsys:locate (system definition-name &key errorp &allow-other-keys)
  (:method :around ((system-name symbol) definition-name &rest keys)
    (%forward-ikeyword #'defsys:locate #'call-next-method
                       system-name definition-name keys))
  (:method ((system-name symbol) definition-name &rest keys)
    (declare (ignore keys))
    (defsys:locate (defsys:locate 'defsys:system system-name)
                   definition-name)))

(defgeneric (setf defsys:locate)
    (new-definition system definition-name &key errorp &allow-other-keys)
  (:method :around (new-definition (system-name symbol) definition-name &rest keys)
           (%forward-ikeyword #'(setf defsys:locate) #'call-next-method
                              system-name definition-name keys new-definition))
  (:method (new-definition (system-name symbol) definition-name &rest keys)
    (setf (apply #'defsys:locate (defsys:locate 'defsys:system system-name)
                 definition-name
                 keys)
          new-definition)))

(defgeneric defsys:unbind (system definition-name &key &allow-other-keys)
  (:method :around ((system-name symbol) definition-name &rest keys)
    (%forward-ikeyword #'defsys:unbind #'call-next-method
                       system-name definition-name keys))
  (:method ((system-name symbol) definition-name &rest keys)
    (apply #'defsys:unbind
           (defsys:locate 'defsys:system system-name)
           definition-name
           keys)))


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
