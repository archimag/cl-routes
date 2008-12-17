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

;;; match

(defgeneric match (map uri &optional bindings))

(defmethod match (map (uri string) &optional (bindings +no-bindings+))
  (match map (puri:parse-uri uri) bindings))

(defmethod match ((map mapper) (uri puri:uri) &optional (bindings +no-bindings+))
  (let ((res (unify (slot-value map 'template)
                    (concatenate 'list
                                 (cdr (puri:uri-parsed-path uri))
                                 (list (make-unify-template 'variable
                                                            'routes:route)))
                    bindings)))
    (if res
        (let ((route (cdar res))
              (bindings (cdr res)))
          (cons route
                (reverse bindings))))))
                
  
        

