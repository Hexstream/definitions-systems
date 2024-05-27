(in-package #:definitions-systems)

(defmethod defsys:replace-definition ((system defsys:system) old-definition new-definition definition-name
                                      &key
                                        ((defsys:unbind-definition unbind-options))
                                        ((defsys:bind-definition bind-options)))
  (apply #'defsys:unbind-definition system old-definition definition-name unbind-options)
  (apply #'defsys:bind-definition system new-definition definition-name bind-options))
