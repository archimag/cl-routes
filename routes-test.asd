;; routes-test.asd

(defsystem :routes-test
    :version "0.1"
    :depends-on (:routes :lift)
    :depends-on (:routes :lift)
    :components ((:module "test"
                          :serial t
                          :components ((:file "test")))))