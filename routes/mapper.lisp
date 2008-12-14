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

;;; match

(defgeneric match (map uri))

(defmethod match (map (uri string))
  (match map (puri:parse-uri uri)))

(defmethod match ((map mapper) (uri puri:uri))
  (let ((res (unify (slot-value map 'template)
                    (concatenate 'list
                                 (cdr (puri:uri-parsed-path uri))
                                 (list (make-unify-template 'variable
                                                            'routes:route))))))
    (if res
        (let ((route (cdar res))
              (bindings (cdr res)))
          (cons route
                (reverse bindings))))))
                
  
        

