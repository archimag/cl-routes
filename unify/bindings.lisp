;;;; bindings.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:routes.unify)

(defconstant +fail+ nil "Indicates unification failure")

(defvar +no-bindings+ '((nil))
  "Indicates unification success, with no variables.")

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (cdr (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (acons var
         val
         (if (eq bindings +no-bindings+)
             nil
             bindings)))

(defun occurs-in-p (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-in-p var (lookup x bindings) bindings))
        ((consp x) (or (occurs-in-p var (first x) bindings)
                       (occurs-in-p var (rest x) bindings)))
        (t nil)))
