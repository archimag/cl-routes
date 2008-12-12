;;; merge.lisp

(in-package :routes.unify)

;;; template=

(defgeneric template= (a b))

(defmethod template= (a b)
  (equal a b))

(defmethod template= ((a variable-template) (b variable-template))
  (eql (template-spec a)
       (template-spec b)))

(defmethod template= ((a unify-template) (b unify-template))
  (if (eql (type-of a)
           (type-of b))
      (template= (template-spec a)
                 (template-spec b))))

(defmethod template= ((a cons) (b cons))
  (and (template= (car a) (car b))
       (template= (cdr a) (cdr b))))

;;; merge-templates/impl

(defgeneric merge-templates/impl (a b))

(defmethod merge-templates/impl (a b)
  (if (template= a b)
      a
      (make-unify-template 'or (list a b))))

(defmethod merge-templates/impl ((a cons) (b cons))
  (if (and (template= (car a) (car b)))
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
                                 (cond ((template= part b) (setq x part))
                                       ((and (consp part)
                                             (consp b)
                                             (template= (car part) (car b)))
                                        (setq x (merge-templates/impl part b)))
                                       (t (collect part into left)))
                                 (collect part into right))
                             (finally (return (if x
                                                  (concatenate 'list left (list x) right)
                                                  (cons b left)))))))

(defmethod merge-template/impl ((a or-template) (b or-template))
  (error "not implemented"))

;;; merge-templates

(defun merge-templates (a &rest b)
  (iter (for tmpl in b)
        (reducing tmpl
                  by #'merge-templates/impl
                  initial-value a)))
