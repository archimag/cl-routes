;;; hunchentoot-routes.lisp

(asdf:operate 'asdf:load-op 'hunchentoot)
(asdf:operate 'asdf:load-op 'cl-who)
(asdf:operate 'asdf:load-op 'routes)

(defun create-route-dispatcher (map)
  (flet ((routes-handler ()
           (funcall (hunchentoot:aux-request-value 'routes-handler)
                    (hunchentoot:aux-request-value 'routes-bindings))))
    (lambda (req)
      (let ((match-result (cdr (routes:match map
                                             (hunchentoot:request-uri req)))))
        (hunchentoot:dispatch-easy-handlers req)
        (if match-result
            (progn
              (setf (hunchentoot:aux-request-value 'routes-handler) (cdr (assoc :handler match-result)))
              (setf (hunchentoot:aux-request-value 'routes-bindings) match-result)
              #'routes-handler))))))

(defparameter *map* (make-instance 'routes:mapper))

(setq hunchentoot:*dispatch-table*
      (list (create-route-dispatcher *map*)
            #'hunchentoot:default-dispatcher))
    
(hunchentoot:start-server :port 8080)

(defun index.html (bindings)
  (let ((count (cdr (assoc :count-chapters bindings))))
    (who:with-html-output-to-string (s)
      (:html 
       (:body 
        (:h1 "Index")
        (:ul (loop for x from 1 to count
                do (who:htm (:li (:a :href (format nil "chapter-~S.html" x)
                                     (who:str (format nil "Chapter ~S" x))))))))))))
  
(defun chapter-?.html (bindings)
  (let ((id (cdr (assoc :id bindings))))
    (who:with-html-output-to-string (s)
      (:html
       (:body
        (:h1 (who:fmt "Chapter ~A" id))
        (:p (who:fmt "This is a chapter number ~A" id))
        (:a :href "index.html" "Back to index"))))))

(routes:connect *map* (routes:make-route "routes/chapter-:(id).html" :extra-bindings '(:handler chapter-?.html)))
(routes:connect *map* (routes:make-route "routes/index.html" :extra-bindings '(:handler index.html :count-chapters 10)))


