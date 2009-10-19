;; apply.lisp

(in-package :routes.unify)

(defgeneric apply-bindings (tmpl bindings))

(defmethod apply-bindings ((tmpl (eql nil)) bindings)
  (declare (ignore bindings))
  nil)

(defmethod apply-bindings ((tmpl string) bindings)
  (declare (ignore bindings))
  tmpl)
    
(defmethod apply-bindings ((tmpl cons) bindings)
  (let ((first (apply-bindings (car tmpl)
                               bindings))
        (rem (apply-bindings (cdr tmpl) bindings)))
    (if (consp first)
        (concatenate 'list
                     first
                     rem)
        (cons first
              rem))))

(defmethod apply-bindings ((var variable-template) bindings)
  (or (lookup (template-spec var) bindings)
      var))

(defmethod apply-bindings ((var wildcard-template) bindings)
  (or (lookup (template-spec var) bindings)
      var))

;; optimize required
(defmethod apply-bindings ((tmpl concat-template) bindings)
  (labels ((simplify (spec)
             (cond
               ((= (length spec) 1) spec)
               ((and (stringp (first spec))
                     (stringp (second spec)))
                (simplify (cons (concatenate 'string
                                             (first spec)
                                             (second spec))
                                (cddr spec))))
               (t (cons (car spec)
                        (simplify (cdr spec)))))))
    (let ((spec (simplify (apply-bindings (template-spec tmpl)
                                          bindings))))
      (if (cdr spec)
          (make-unify-template 'concat spec)
          (car spec)))))

  
  