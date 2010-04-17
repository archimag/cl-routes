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
                        :components ((:file "unify")))
               (:module "routes"
                        :components ((:file "package")                                     
                                     (:file "route" :depends-on ("package"))
                                     (:file "mapper" :depends-on ("route"))
                                     #+swank (:file "routes-swank" :depends-on ("mapper")))
                        :depends-on ("unify"))))
