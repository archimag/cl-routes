;; routes.lisp

(in-package :routes)

;;; route

(defclass route ()
  ((template :initarg :template)
   (extra-bindings :initform nil :initarg :extra-bindings)
   (conditions :initform nil :initarg :conditions)))

;;; check-conditions

(defgeneric route-check-conditions (route bindings))

(defmethod route-check-conditions ((route route) bindings)
  (let ((conditions (slot-value route 'conditions)))
    (if conditions
        (iter (for condition in conditions)
              (unless (funcall (cdr condition)
                               (routes.unify::lookup (car condition)
                                                     bindings))
                (return nil))
              (finally (return t)))
        t)))

;;; extend-binginds

(defgeneric route-extend-bindings (route bindings))

(defmethod route-extend-bindings ((route route) bindings)
  (let ((extra-bindings (slot-value route 'extra-bindings)))
    (if extra-bindings
        (iter (for x in extra-bindings)
              (reducing x
                        by (lambda (res pair)
                             (unify (make-unify-template 'variable (car pair))
                                    (cdr pair)
                                    res))
                        initial-value bindings))
        bindings)))

;;; make-route

(defun parse-path (str)
  (flet ((mkvar (name)
           (make-unify-template 'variable (intern (string-upcase name) :keyword))))
    (if (> (length str) 0)
        (let ((pos (position #\: str)))
          (if pos
              (let ((rest (if (char= (elt str (1+ pos)) #\()
                              (let ((end (position #\) str :start (1+ pos))))
                                (if end
                                    (cons (mkvar (subseq str (+ pos 2) end))
                                          (handler-case
                                              (parse-path (subseq str (1+ end)))
                                            (condition () (error "Bad format of the ~S" str))))
                                    (error "Bad format of the ~S" str)))
                              (list (mkvar (subseq str (1+ pos)))))))
                (if (> pos 0)
                    (cons (subseq str 0 pos) rest)
                    rest))
              (list str))))))

(defun make-route (tmpl &key extra-bindings conditions)
  (make-instance 'route
                 :template (iter (for path in (split-sequence #\/ tmpl :remove-empty-subseqs t))
                                 (collect (let ((spec (parse-path path)))
                                            (if (cdr spec)
                                                (make-unify-template 'unify::concat spec)
                                                (car spec)))))
                 :extra-bindings (iter (for rest on extra-bindings by #'cddr)
                                       (collect (cons (car rest)
                                                      (cadr rest))))
                 :conditions (iter (for rest on conditions by #'cddr)
                                   (collect (cons (car rest)
                                                  (cadr rest))))))

;;; unify/impl for route

(defmethod unify::unify/impl ((b route) (a unify::variable-template) bindings)
  (unify::unify a b bindings))
  
(defmethod unify::unify/impl ((a unify::variable-template) (route route) bindings)
  (if (route-check-conditions route bindings)
      (call-next-method a
                        route
                        (route-extend-bindings route bindings))))


