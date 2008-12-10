;; routes.asd

(defsystem :routes
  :version "0.1"
  :depends-on (:puri)
  :components ((:module "unify"
                        :serial t
                        :components ((:file "package")
                                     (:file "unify" :depends-on ("package"))))
               (:module "routes"
                        :serial t
                        :components ((:file "package")
                                     (:file "routes")))))
