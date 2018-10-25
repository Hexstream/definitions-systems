(in-package #:definitions-systems)

(defun %forward-ikeyword (forward continue system definition-name &rest keys)
  (if (ikeywords:ikeywordp definition-name)
      (apply forward
             system
             (intern (symbol-name definition-name) #.(find-package '#:keyword))
             keys)
      (funcall continue)))

(defun (setf %forward-ikeyword) (new forward continue system definition-name &rest keys)
  (if (ikeywords:ikeywordp definition-name)
      (apply forward
             new
             system
             (intern (symbol-name definition-name) #.(find-package '#:keyword))
             keys)
      (funcall continue)))

(defmethod defsys:locate :around ((system defsys:ikeywords-mixin) (definition-name symbol) &key (errorp t))
  (%forward-ikeyword #'defsys:locate #'call-next-method system definition-name :errorp errorp))

(defmethod (setf defsys:locate) :around (new-definition (system defsys:ikeywords-mixin) (definition-name symbol) &key (errorp nil))
  (setf (%forward-ikeyword #'(setf defsys:locate) #'call-next-method
                           system definition-name :errorp errorp)
        new-definition))

(defmethod defsys:unbind :around ((system defsys:ikeywords-mixin) (definition-name symbol))
  (%forward-ikeyword #'defsys:unbind #'call-next-method system definition-name))

(defmethod defsys:boundp :around ((system defsys:ikeywords-mixin) (definition-name symbol))
  (%forward-ikeyword #'defsys:boundp #'call-next-method system definition-name))

(defmethod defsys:expand-definition :around ((system defsys:ikeywords-mixin) (definition-name symbol)
                                             environment args &rest options &key &allow-other-keys)
  (apply #'%forward-ikeyword #'defsys:expand-definition #'call-next-method
         system definition-name environment args options))
