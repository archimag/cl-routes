;;;; routes.asd
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:routes-system
  (:use #:cl #:asdf))

(in-package #:routes-system)

(defsystem routes
  :depends-on (#:puri #:iterate #:split-sequence)
  :components ((:module "unify"
                        :components ((:file "package")
                                     (:file "bindings" :depends-on ("package"))
                                     (:file "unify" :depends-on ("bindings"))
                                     (:file "merge" :depends-on ("unify"))
                                     (:file "apply" :depends-on ("bindings"))))
               (:module "routes"
                        :components ((:file "package")
                                     (:file "route" :depends-on ("package"))
                                     (:file "mapper" :depends-on ("route")))
                        :depends-on ("unify"))))
