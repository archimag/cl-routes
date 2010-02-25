;;;; routes-test.asd
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defsystem :routes-test
    :version "0.1"
    :depends-on (:routes :lift)
    :depends-on (:routes :lift)
    :components ((:module "test"
                          :serial t
                          :components ((:file "test")))))