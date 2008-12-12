;; routes.lisp

(in-package :routes)

;;; routes

(defclass route ()
  ((template :initarg :template)))


;;; make-routes

(defun parse-path (str)
  (flet ((mkvar (name) (intern (string-upcase name))))
    (if (> (length str) 0)
        (let ((pos (position #\: str)))
          (if pos
              (let ((rest (if (char= (elt str (1+ pos)) #\{)
                              (let ((end (position #\} str :start (1+ pos))))
                                (if end
                                    (cons (make-unify-template 'variable
                                                               (mkvar (subseq str (+ pos 2) end)))
                                          (handler-case
                                              (parse-path (subseq str (1+ end)))
                                            (condition () (error "Bad format of the ~S" str))))
                                    (error "Bad format of the ~S" str)))
                              (list (make-unify-template 'variable
                                                         (mkvar (subseq str (1+ pos))))))))
                (if (> pos 0)
                    (cons (subseq str 0 pos) rest)
                    rest))
              (list str))))))

(defun make-route (tmpl)
  (make-instance 'route
                 :template (iter (for path in (split-sequence #\/ tmpl :remove-empty-subseqs t))
                                 (collect (let ((spec (parse-path path)))
                                            (if (cdr spec)
                                                (make-unify-template 'unify::concat spec)
                                                (car spec)))))))

;;; match

(defun match (route uri)
  (unify (slot-value route 'template)
         (cdr (puri:uri-parsed-path uri))))



