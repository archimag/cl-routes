;;; mapper.lisp

(in-package :routes)

;;; mapper

(defclass mapper ()
  ((template :initform nil :initarg :template)))

;;; connect

(defun connect (map route)
  (let ((spec (slot-value map 'template))
        (route-spec (concatenate 'list
                                 (slot-value route 'template)
                                 (list route))))
    (setf (slot-value map 'template)
          (if spec
              (merge-templates spec route-spec)
              (concatenate 'list
                           route-spec)))))

;;; reset

(defun reset-mapper (map)
  (setf (slot-value map 'template) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; match
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric match (map uri &optional bindings))

;;; match (map (empty (eql nil)

(defmethod match (map (empty (eql nil)) &optional (bindings +no-bindings+))
  (match map '(nil) bindings))

;;; match (map (uri string))

(defmethod match (map (uri string) &optional (bindings +no-bindings+))
  (if (string= uri "/")
      (match map nil bindings)
      (match map (puri:parse-uri uri) bindings)))

;;; match (map (uri puri:uri))

(defmethod match (map (uri puri:uri) &optional (bindings +no-bindings+))
  (match map (cdr (puri:uri-parsed-path uri)) bindings))

;;; match (map (route routes))

(defmethod match (map (route route) &optional (bindings +no-bindings+))
  (match map
         (slot-value route 'template)
         (iter (for pair in (slot-value route 'extra-bindings))
               (reducing pair
                         by #'(lambda (b p) (extend-bindings (car p) (cdr p) b))
                         initial-value bindings))))

;;; match (map (paths cons))

(defmethod match (map (paths cons) &optional (bindings +no-bindings+))
  (let ((res (unify (slot-value map 'template)
                    (if (car paths)
                        (concatenate 'list
                                     (apply-bindings paths bindings)
                                     (list (make-unify-template 'variable
                                                                'routes:route)))
                        (list (make-unify-template 'variable
                                                   'routes:route)))
                    bindings)))
    (if res
        (let ((route (cdar res))
              (bindings (cdr res)))
          (cons route
                (reverse bindings))))))
          
