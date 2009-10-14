;; package.lisp

(defpackage :routes.unify
  (:use :cl :iter)
  (:export
   :unify
   :make-unify-template
   :merge-templates
   :+no-bindings+
   :extend-bindings
   :apply-bindings
   :template-variables
   ))