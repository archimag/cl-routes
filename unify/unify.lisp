;; unify.lisp

;;;; Unification and Substitutions (based on code from AIMA by Peter Norvig)

(in-package :routes.unify)

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

;;; variable-template

(define-unify-template variable)

(defun variable-p (x)
  (typep x 'variable-template))

;;; or-template

(define-unify-template or)

;;; concat template

(defclass concat-template (unify-template) ())

(defmethod make-unify-template ((key (eql 'concat)) spec)
  (if (cdr spec)
      (make-instance 'concat-template :spec spec)
      (car spec)))

(defmethod make-unify-template ((key (eql 'concat)) (spec (eql nil)))
  nil)

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bindings (substitutions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +fail+ nil "Indicates unification failure")

(defvar +no-bindings+ '((nil))
  "Indicates unification success, with no variables.")


(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy +no-bindings+
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unify/impl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric unify/impl (a b bindings))

(defun unify (x y &optional (bindings +no-bindings+))
  (if bindings
      (unify/impl x y bindings)))

;;; unify/impl for cons

(defmethod unify/impl ((a cons) (b cons) bindings)
  (unify (rest a) (rest b)
         (unify (first a) (first b) bindings)))

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
                              (unify first-spec
                                     (subseq str 0 pos)
                                     (unify (make-unify-template 'concat (cddr spec))
                                            (if (> (length str)
                                                   (+ (length second-spec) pos))
                                                (subseq str (+ (length second-spec) pos)))
                                            bindings))
                              +fail+)))
        (t +fail+)))))

;; unify/impl for or-template

(defmethod unify/impl (a (b or-template) bindings)
  (unify b a bindings))

(defmethod unify/impl ((tmpl or-template) x bindings)
  (print "hello")
  (let ((spec (template-spec tmpl)))
    (or (unify (car spec) x bindings)
        (if (cdr spec)
            (unify (make-unify-template 'or (cdr spec)) x bindings)))))

;;; default unify/impl    

(defmethod unify/impl (x y bindings)
  (if (eql x y) bindings +fail+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #$ reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sharp-$-reader (stream chr arg)
  (declare (ignore chr arg))
  (let ((spec (read stream nil nil t)))
    (typecase spec
      (cons (make-unify-template (find-symbol (symbol-name (first spec)) (find-package :unify))
                                 (cdr spec)))
      (symbol (make-unify-template 'variable spec))
      (t (error "bad format of tempalte: ~A" spec)))))

(set-dispatch-macro-character #\# #\$ #'unify::sharp-$-reader)
