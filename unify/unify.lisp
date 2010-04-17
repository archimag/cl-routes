;;;; unify.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


;;;; Unification and Substitutions (based on code from AIMA by Peter Norvig)

(in-package #:routes.unify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unify-template
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass unify-template ()
  ((spec :initarg :spec :accessor template-spec)))

(defun template-p (x)
  (typep x 'unify-template))

(defgeneric make-unify-template (key spec))

(defmacro define-unify-template (key)
  (let ((name (intern (string-upcase (format nil "~S-template" key)))))
    `(progn
       (defclass ,name (unify-template) ())
       (defmethod make-unify-template ((key (eql (quote ,key))) spec)
         (make-instance (quote ,name) :spec spec)))))

;;; template-variables

(defgeneric template-variables (tmpl))

(defmethod template-variables (tmpl)
  nil)

(defmethod template-variables ((tmpl (eql nil)))
  nil)

(defmethod template-variables ((tmpl cons))
  (concatenate 'list
               (template-variables (car tmpl))
               (template-variables (cdr tmpl))))

(defmethod template-variables ((tmpl unify-template))
  (template-variables (template-spec tmpl)))

;;; variable-template

(define-unify-template variable)

(defmethod print-object ((tmpl variable-template) stream)
  (format stream "#$~S" (template-spec tmpl)))

(defmethod template-variables ((tmpl variable-template))
  (list (template-spec tmpl)))

(defun variable-p (x)
  (typep x 'variable-template))

(defclass custom-variable-template (variable-template)
  ((parse-fun :initarg :parse :initform nil :reader custom-variable-parse-fun)))

;;; or-template

(define-unify-template or)

(defmethod print-object ((tmpl or-template) stream)
  (format stream "#$(OR ~{~A~^ ~})" (template-spec tmpl)))

;;; concat template

(defclass concat-template (unify-template) ())

(defmethod make-unify-template ((key (eql 'concat)) spec)
  (if (cdr spec)
      (make-instance 'concat-template :spec spec)
      (car spec)))

(defmethod make-unify-template ((key (eql 'concat)) (spec (eql nil)))
  nil)

(defmethod print-object ((tmpl concat-template) stream)
  (format stream "#$(CONCAT ~{~A~^ ~})" (template-spec tmpl)))

;;; remainder template

(define-unify-template wildcard)

(defmethod print-object ((tmpl wildcard-template) stream)
  (format stream "#@~S" (template-spec tmpl)))

(defmethod template-variables ((tmpl wildcard-template))
  (list (template-spec tmpl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unify/impl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric unify/impl (a b bindings))

(defun unify (x y &optional (bindings +no-bindings+))
  (if bindings
      (unify/impl x y bindings)))

;;; unify/impl default implementation

(defmethod unify/impl (a b bindings)
  (if (equal a b) bindings +fail+))


;;; unify/impl for wildcard

(defmethod unify/impl ((tmpl wildcard-template) x bindings &aux (var (template-spec tmpl)))
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((occurs-in-p var x bindings)
         +fail+)
        (t (extend-bindings var x bindings))))


;;; unify/impl for cons

(defmethod unify/impl ((a cons) (b cons) bindings)
  (let ((wildcard-p-1 (typep (first a) 'wildcard-template))
        (wildcard-p-2 (typep (first b) 'wildcard-template)))
    (cond
      ((and wildcard-p-1 wildcard-p-2) nil)
      (wildcard-p-1 (let ((len (- (length b)
                                  (length (cdr a)))))
                      (if (not (minusp len))
                          (unify (cdr a)
                                 (subseq b len)
                                 (unify (first a)
                                        (subseq b 0 len)
                                        bindings)))))
      (wildcard-p-2 (unify/impl b a bindings))
      (t (unify (rest a)
                (rest b)
                (unify (first a)
                       (first b)
                       bindings))))))

;;; unify/impl for strings

(defmethod unify/impl ((a string) (b string) bindings)
  (if (string= a b) bindings +fail+))

;;; unify/impl variable-template

(defmethod unify/impl (a (b variable-template) bindings)
  (unify b a bindings))

(defmethod unify/impl ((tmpl variable-template) x bindings &aux (var (template-spec tmpl)))
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((occurs-in-p var x bindings)
         +fail+)
        (t (extend-bindings var x bindings))))

;;; unify/impl custom-variable-template

(defmethod unify/impl ((tmpl custom-variable-template) x bindings)
  (let ((parsed-value (ignore-errors
                        (funcall (custom-variable-parse-fun tmpl)
                                 x))))
    (if parsed-value
        (call-next-method tmpl
                          parsed-value
                          bindings)
        +fail+)))
                      
                        
  
;;; unify/impl concat-template

(defmethod unify/impl (a (b concat-template) bindings)
  (unify b a bindings))

(defmethod unify/impl ((tmpl concat-template) (str string) bindings)
  (flet ((remove-prefix (str prefix)
           (let ((prefix-length (length prefix)))
             (if (and (> (length str) prefix-length)
                      (string= (subseq str 0 prefix-length) prefix))
                 (subseq str prefix-length)))))
    (let* ((spec (template-spec tmpl))
           (first-spec (car spec)))
      (typecase first-spec
        (string (unify (remove-prefix str first-spec)
                       (make-unify-template 'concat (cdr spec))
                       bindings))
        (unify-template (let* ((second-spec (second spec))
                               (pos (search second-spec str)))
                          (if pos
                              (unify (make-unify-template 'concat (cddr spec))
                                     (if (> (length str)
                                            (+ (length second-spec) pos))
                                         (subseq str (+ (length second-spec) pos)))
                                     (unify first-spec
                                            (subseq str 0 pos)
                                            bindings))
                              +fail+)))
        (t +fail+)))))

;; unify/impl for or-template

(defmethod unify/impl (a (b or-template) bindings)
  (unify b a bindings))

(defmethod unify/impl ((tmpl or-template) x bindings)
  (let ((spec (template-spec tmpl)))
    (iter (for item in spec)
          (with result = nil)
          (with result-variable-count = -1)
          (let* ((item-unify-result (unify item x bindings))
                 (count (if item-unify-result (length item-unify-result))))
            (if (and item-unify-result
                     (not (find (car item)
                           '(variable-template or-template concat-template wildcard-template)
                           :test #'typep)))
                (return item-unify-result))
            (if (and count (> count result-variable-count))
                (progn 
                  (setq result item-unify-result)
                  (setq result-variable-count count))))
          (finally (return result)))))

;;; default unify/impl    

(defmethod unify/impl (x y bindings)
  (if (eql x y) bindings +fail+))

