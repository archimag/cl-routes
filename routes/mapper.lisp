;;; mapper.lisp

(in-package :routes)

;;; mapper

(defclass mapper ()
  ((template :initform nil :initarg :template)))

(defun connect (map route)
  (let ((spec (slot-value map 'template))
        (route-spec (slot-value route 'template)))
    (setf (slot-value map 'template)
          (if spec
              (merge-templates spec route-spec)
              route-spec))))

;;; match

(defmethod match ((map mapper) uri)
  (unify (slot-value map 'template)
         (cdr (puri:uri-parsed-path uri))))
  
        

