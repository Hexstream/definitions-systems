(in-package #:definitions-systems)

(defmethod defsys:locate :around (system definition-name &key (errorp t))
  (or (call-next-method)
      (when errorp
        (error 'defsys:not-found :system system :name definition-name))))

(defmethod (setf defsys:locate) (new-definition (system defsys:system) definition-name
                                 &key
                                   ((defsys:locate locate-options))
                                   ((defsys:bind-definition bind-options) nil bind-options-p)
                                   ((defsys:replace-definition replace-options)))
  (let ((old-definition (apply #'defsys:locate system definition-name :errorp nil locate-options)))
    (if old-definition
        (if (eq new-definition old-definition)
            new-definition
            (apply #'defsys:replace-definition system old-definition new-definition definition-name
                   (if bind-options-p
                       (append replace-options
                               (list 'defsys:bind-definition bind-options))
                       replace-options)))
        (apply #'defsys:bind-definition system new-definition definition-name bind-options))))

(defmethod defsys:unbind ((system defsys:system) definition-name
                          &key
                            ((defsys:locate locate-options))
                            ((defsys:unbind-definition unbind-options)))
  (let ((definition (apply #'defsys:locate system definition-name :errorp nil locate-options)))
    (when definition
      (apply #'defsys:unbind-definition system definition definition-name unbind-options))))

(defmethod defsys:count ((system defsys:system) &key ((defsys:map map-options)))
  (warn "Using slow ~S." 'defsys:count)
  (let ((count 0))
    (apply #'defsys:map
           (lambda (name definition)
             (declare (ignore name definition))
             (incf count))
           system
           map-options)
    count))
