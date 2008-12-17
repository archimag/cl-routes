;;; hunchentoot-routes.lisp

(defpackage :routes.hunchentoot
  (:use :cl :split-sequence :iter :routes )
  (:export :make-dispatcher
           :connect-handler))

(in-package :routes.hunchentoot)

;;; hunchentoot-route

(defclass hunchentoot-route (route)
  ((handler :initarg :handler :reader route-handler)))

;;; make-dispatcher

(defun request-traits (req)  
  (acons :host (hunchentoot:host req)
         (acons :method (hunchentoot:request-method req)
                nil)))

(defun make-dispatcher (map)
  (flet ((routes-handler ()
           (funcall (hunchentoot:aux-request-value 'routes-handler)
                    (hunchentoot:aux-request-value 'routes-bindings))))
    (lambda (req)
      (let ((match-result (routes:match map
                                        (hunchentoot:request-uri req)
                                        (request-traits req))))
        (if match-result 
            (progn
              (setf (hunchentoot:aux-request-value 'routes-handler) (route-handler (car match-result)))
              (setf (hunchentoot:aux-request-value 'routes-bindings) (cdr match-result))
              #'routes-handler))))))

;;; connect-handler

(defun connect-handler (mapper tmpl handler &key method host extra-bindings conditions)
  (let ((bindings extra-bindings))    
    (if method
        (setq bindings (acons :method method bindings)))
    (if host
        (setq bindings (acons :host host bindings)))
    (print bindings)
    (connect mapper
             (make-instance 'hunchentoot-route                            
                            :template (iter (for path in (split-sequence #\/ tmpl :remove-empty-subseqs t))
                                            (collect (let ((spec (routes::parse-path path)))
                                                       (if (cdr spec)
                                                           (unify:make-unify-template 'unify::concat spec)
                                                           (car spec)))))
                            :handler handler
                            :extra-bindings bindings
                            :conditions conditions))))