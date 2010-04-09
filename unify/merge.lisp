;;;; merge.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:routes.unify)

;;; uri-template-equal

(defgeneric uri-template-equal (a b))

(defmethod uri-template-equal (a b)
  (equal a b))

(defmethod uri-template-equal ((a variable-template) (b variable-template))
  (eql (template-spec a)
       (template-spec b)))

(defmethod uri-template-equal ((a unify-template) (b unify-template))
  (if (eql (type-of a)
           (type-of b))
      (uri-template-equal (template-spec a)
                 (template-spec b))))

(defmethod uri-template-equal ((a cons) (b cons))
  (and (uri-template-equal (car a) (car b))
       (uri-template-equal (cdr a) (cdr b))))

;;; merge-templates/impl

(defgeneric merge-templates/impl (a b))

(defmethod merge-templates/impl (a b)
  (if (uri-template-equal a b)
      a
      (make-unify-template 'or (list a b))))

(defmethod merge-templates/impl ((a cons) (b cons))
  (if (and (uri-template-equal (car a) (car b)))
      (cons (car a)
            (merge-templates/impl (cdr a)
                                  (cdr b)))
      (make-unify-template 'or (list a b))))


(defmethod merge-templates/impl (a (b or-template))
  (merge-templates/impl b a))

(defmethod merge-templates/impl ((a or-template) b)
  (make-unify-template 'or
                       (iter (for part in (template-spec a))
                             (with x = nil)
                             (if (not x)
                                 (cond ((uri-template-equal part b) (setq x part))
                                       ((and (consp part)
                                             (consp b)
                                             (uri-template-equal (car part) (car b)))
                                        (setq x (merge-templates/impl part b)))
                                       (t (collect part into left)))
                                 (collect part into right))
                             (finally (return (if x
                                                  (concatenate 'list left (list x) right)
                                                  (cons b left)))))))

(defmethod merge-templates/impl ((a or-template) (b or-template))
  (error "not implemented"))

;;; merge-templates

(defun merge-templates (a &rest b)
  (iter (for tmpl in b)
        (reducing tmpl
                  by #'merge-templates/impl
                  initial-value a)))
