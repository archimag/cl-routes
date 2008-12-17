;; hunchentoot-routes.asd

(defsystem :hunchentoot-routes
  :version "0.2"
  :depends-on (:routes :hunchentoot)
  :components ((:module "hunchentoot" 
                        :serial t
                        :components ((:file "hunchentoot-routes")))))
