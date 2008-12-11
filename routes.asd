;; routes.asd

(defsystem :routes
  :version "0.1"
  :depends-on (:puri :iterate :split-sequence)
  :components ((:module "unify"
                        :serial t
                        :components ((:file "package")
                                     (:file "unify" :depends-on ("package"))))
               (:module "routes"
                        :serial t
                        :components ((:file "package")
                                     (:file "parse" :depends-on ("package"))
                                     (:file "routes" :depends-on ("parse")))
                        :depends-on ("unify"))))
