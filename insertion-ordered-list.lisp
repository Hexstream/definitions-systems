(in-package #:definitions-systems)

(defclass %insertion-ordered-list ()
  ((%list :reader %list
          :type list
          :initform (list '%list))
   (%tail :accessor %tail
          :type cons)
   (%hash2 :initarg :hash-table
           :reader %hash2
           :type hash-table
           :initform (make-hash-table :test 'eql))))

(defmethod shared-initialize :after ((ordered-list %insertion-ordered-list) slot-names &key)
  (when (or (eq slot-names t) (member '%list slot-names))
    (setf (%tail ordered-list) (last (%list ordered-list)))))

(defgeneric %insert (ordered-list key value)
  (:method ((ordered-list %insertion-ordered-list) key value)
    (let* ((hash (%hash2 ordered-list))
           (previous-cons (gethash key hash)))
      (if previous-cons
          (setf (second previous-cons) value)
          (let ((new-tail (list value))
                (former-tail (%tail ordered-list)))
            (setf (%tail ordered-list) new-tail
                  (cdr former-tail) new-tail
                  (gethash key hash) former-tail))))
    value))

(defgeneric %remove (ordered-list key)
  (:method ((ordered-list %insertion-ordered-list) key)
    (let* ((hash (%hash2 ordered-list))
           (previous-cons (gethash key hash)))
      (when previous-cons
        (let ((next-cons (cddr previous-cons)))
          (setf (cdr previous-cons) next-cons)
          (unless next-cons
            (setf (%tail ordered-list) previous-cons)))
        (remhash key hash)))))

(defgeneric %map (function ordered-list)
  (:method (function (ordered-list %insertion-ordered-list))
    (mapc function (cdr (%list ordered-list)))
    nil)
  (:argument-precedence-order ordered-list function))
