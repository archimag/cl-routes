;;;; routes.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(in-package #:routes)

;;; route

(defclass route ()
  ((template :initarg :template)))

;;; check-conditions

(defgeneric route-check-conditions (route bindings)
  (:method ((route route) bindings)
    t))

;;; make-route

(defun parse-path (str)
  (flet ((mkvar (name)
           (make-unify-template 'variable (intern (string-upcase name) :keyword))))
    (if (> (length str) 0)
        (if (char= (char str 0)
                   #\*)
            (list (make-unify-template 'routes.unify::wildcard
                                       (intern (string-upcase (subseq str 1)) :keyword)))
            (let ((pos (position #\: str)))
              (if pos
                  (let ((rest (if (char= (elt str (1+ pos)) #\()
                                  (let ((end (position #\) str :start (1+ pos))))
                                    (if end
                                        (cons (mkvar (subseq str (+ pos 2) end))
                                              (if (< (1+ end) (length str))
                                                  (handler-case
                                                      (parse-path (subseq str (1+ end)))
                                                    (condition () (error "Bad format of the ~S" str)))))
                                        (error "Bad format of the ~S" str)))
                                  (list (mkvar (subseq str (1+ pos)))))))
                    (if (> pos 0)
                        (cons (subseq str 0 pos) rest)
                        rest))
                  (list str))))
        '(""))))

(defun plist->alist (plist)
  (iter (for rest on plist by #'cddr)
        (collect (cons (car rest)
                       (cadr rest)))))

(defun split-template (tmpl)
  (split-sequence #\/ (string-left-trim "/" tmpl)))

(defun parse-template (tmpl)
  (iter (for path in (split-template (string-left-trim "/" tmpl)))
        (collect (let ((spec (routes::parse-path path)))
                   (if (cdr spec)
                       (routes.unify:make-unify-template 'routes.unify::concat spec)
                       (car spec))))))

(defun make-route (tmpl)
  (make-instance 'route
                 :template (parse-template tmpl)))

;;; route-variables

(defun route-variables (route)
  (routes.unify:template-variables (slot-value route
                                               'template)))

;;; unify/impl for route

(defmethod routes.unify::unify/impl ((b route) (a routes.unify::variable-template) bindings)
  (routes.unify::unify a b bindings))
  
(defmethod routes.unify::unify/impl ((a routes.unify::variable-template) (route route) bindings)
  (if (route-check-conditions route bindings)
      (call-next-method a
                        route
                        bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate-url
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-url (route bindings)
  (format nil
          "~{~A~^/~}"
          (apply-bindings (slot-value route 'template)
                          bindings)))
  
