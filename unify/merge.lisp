;;;; merge.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:routes.unify)

;;; uri-template-equal

(defgeneric uri-template-equal (a b)
  (:documentation "Return T if template a and template are equal"))

(defmethod uri-template-equal (a b)
  "Default implementation"
  (equal a b))

(defmethod uri-template-equal ((a unify-template) (b unify-template))
  (and (eql (type-of a)
            (type-of b))
       (uri-template-equal (template-spec a)
                           (template-spec b))))

(defmethod uri-template-equal ((a cons) (b cons))
  (and (uri-template-equal (car a)
                           (car b))
       (uri-template-equal (cdr a) 
                           (cdr b))))

;;; merge-uri-templates

(defgeneric merge-uri-templates (a b)
  (:documentation "Merge the templates A and B into one template"))

(defmethod merge-uri-templates (a b)
  (if (uri-template-equal a b)
      a
      (make-unify-template 'or (list a b))))

(defmethod merge-uri-templates ((a cons) (b cons))
  (if (uri-template-equal (car a) (car b))
      (cons (car a)
            (merge-uri-templates (cdr a)
                                 (cdr b)))
      (make-unify-template 'or (list a b))))


(defmethod merge-uri-templates (a (b or-template))
  (merge-uri-templates b a))

(defmethod merge-uri-templates ((a or-template) b)
  (make-unify-template 'or
                       (iter (for part in (template-spec a))
                             (with x = nil)
                             (if (not x)
                                 (cond ((uri-template-equal part b) (setq x part))
                                       ((and (consp part)
                                             (consp b)
                                             (uri-template-equal (car part) (car b)))
                                        (setq x (merge-uri-templates part b)))
                                       (t (collect part into left)))
                                 (collect part into right))
                             (finally (return (if x
                                                  (concatenate 'list left (list x) right)
                                                  (cons b left)))))))

(defmethod merge-uri-templates ((a or-template) (b or-template))
  (error "not implemented"))

