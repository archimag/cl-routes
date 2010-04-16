;;;; package.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:routes.unify
  (:use #:cl #:iter)
  (:export #:unify
           #:make-unify-template
           #:merge-uri-templates
           #:+no-bindings+
           #:extend-bindings
           #:apply-bindings
           #:template-variables
           #:template-spec))