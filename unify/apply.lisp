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
  (cons (apply-bindings (car tmpl)
                        bindings)
        (apply-bindings (cdr tmpl) bindings)))

(defmethod apply-bindings ((var variable-template) bindings)
  (or (lookup (template-spec var) bindings)
      var))

;; optimize required
(defmethod apply-bindings ((tmpl concat-template) bindings)
  (make-unify-template 'concat
                       (apply-bindings (template-spec tmpl)
                                       bindings)))

  
  