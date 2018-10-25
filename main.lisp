(in-package #:definitions-systems)

(defclass defsys:system () ())


(defgeneric defsys:name (system))

(defclass defsys:name-mixin ()
  ((%name :initarg :name
          :reader defsys:name
          :type symbol)))

(defmethod print-object ((mixin name-mixin) stream)
  (print-unreadable-object (mixin stream :type t)
    (prin1 (defsys:name mixin) stream)))


(defclass defsys:hash-table-mixin ()
  ((%hash :type hash-table :initform (make-hash-table :test 'eq))))


(defclass defsys:standard-system (defsys:name-mixin defsys:hash-table-mixin)
  ())

(defvar *root-system* (make-instance 'defsys:standard-system :name 'defsys:system))


(defun %forward-ikeyword (forward continue system-name definition-name &rest keys)
  (if (ikeywords:ikeywordp system-name)
      (apply forward
             (intern (symbol-name system-name) #.(find-package '#:keyword))
             definition-name
             keys)
      (funcall continue)))

(defun (setf %forward-ikeyword) (new forward continue system-name definition-name &rest keys)
  (if (ikeywords:ikeywordp system-name)
      (apply forward
             new
             (intern (symbol-name system-name) #.(find-package '#:keyword))
             definition-name
             keys)
      (funcall continue)))

(defgeneric defsys:locate (system definition-name &key errorp)
  (:method :around (system definition-name &key (errorp t))
    (or (call-next-method)
        (when errorp
          (error 'defsys:not-found :system system :name definition-name))))
  (:method :around ((system-name symbol) definition-name &key (errorp t))
    (%forward-ikeyword #'defsys:locate #'call-next-method
                       system-name definition-name :errorp errorp))
  (:method ((system-name symbol) definition-name &key (errorp t))
    (defsys:locate (defsys:locate 'defsys:system system-name)
                   definition-name :errorp errorp))
  (:method ((system-name (eql 'defsys:system)) definition-name &key (errorp t))
    (declare (ignore errorp))
    *root-system*)
  (:method ((system defsys:hash-table-mixin) definition-name &key (errorp t))
    (declare (ignore errorp))
    (identity (gethash definition-name (slot-value system '%hash)))))

(defgeneric (setf defsys:locate) (new-definition system definition-name &key errorp)
  (:method :around (new-definition (system-name symbol) definition-name &key (errorp nil))
           (setf (%forward-ikeyword #'(setf defsys:locate) #'call-next-method
                                    system-name definition-name :errorp errorp)
                 new-definition))
  (:method (new-definition (system-name symbol) definition-name &key (errorp nil))
    (setf (defsys:locate (defsys:locate 'defsys:system system-name)
                         definition-name
                         :errorp errorp)
          new-definition))
  (:method (new-definition (system defsys:hash-table-mixin) definition-name &key (errorp nil))
    (declare (ignore errorp))
    (setf (gethash definition-name (slot-value system '%hash))
          new-definition)))

(defgeneric defsys:unbind (system definition-name)
  (:method :around ((system-name symbol) definition-name)
    (%forward-ikeyword #'defsys:unbind #'call-next-method
                       system-name definition-name))
  (:method ((system-name symbol) definition-name)
    (defsys:unbind (defsys:locate 'defsys:system system-name)
                   definition-name))
  (:method ((system hash-table-mixin) definition-name)
    (setf (gethash definition-name (slot-value system '%hash)) nil)))

(defgeneric defsys:boundp (system definition-name)
  (:method (system definition-name)
    (not (null (defsys:locate system definition-name :errorp nil)))))


(defgeneric defsys:expand-definition (system definition-name environment args
                                      &rest options &key &allow-other-keys)
  (:method :around ((system-name symbol) definition-name environment args
                    &rest options &key &allow-other-keys)
    (apply #'%forward-ikeyword #'defsys:expand-definition #'call-next-method
           system-name definition-name environment args options))
  (:method ((system-name symbol) definition-name environment args
            &rest options &key &allow-other-keys)
    (apply #'defsys:expand-definition
           (defsys:locate 'defsys:system system-name)
           definition-name environment args options)))

(defmacro defsys:define ((kind definition-name &body options)
                         &body args &environment env)
  (apply #'defsys:expand-definition kind definition-name env args options))


(define-condition defsys:not-found (error)
  ((%system :initarg :system
            :reader defsys:system)
   (%name :initarg :name
          :reader defsys:name))
  (:report (lambda (condition stream)
             (format stream "No definition named ~S in system ~S."
                     (defsys:name condition) (defsys:system condition)))))
