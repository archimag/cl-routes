
(asdf:operate 'asdf:load-op 'cl-who)
(asdf:operate 'asdf:load-op 'hunchentoot-routes)

(defparameter *port* 8088)

(defparameter *map* (make-instance 'routes:mapper))

(setq hunchentoot:*dispatch-table*
      (list (routes.h:make-dispatcher *map*)
            #'hunchentoot:default-dispatcher))

(defparameter *server* (hunchentoot:start-server :port *port*))

;;; first vhost

(defun index.html ()
  (who:with-html-output-to-string (s)
    (:html 
     (:body 
      (:h1 "Index")
      (:ul (loop for x from 1 to 10
              do (who:htm (:li (:a :href (format nil "chapter-~S.html" x)
                                   (who:fmt "Chapter ~S" x))))))))))
  
(defun chapter-?.html ()
  (let ((id (routes.h:route-parameter :id)))
    (who:with-html-output-to-string (s)
      (:html
       (:body
        (:h1 (who:fmt "Chapter ~A" id))
        (:p (who:fmt "This is a chapter ~A" id))
        (:ul (loop for x from 1 to 10
                do (who:htm (:li (:a :href (format nil "chapter-~A-~A.html" id x)
                                     (who:fmt "Chapter ~A-~A" id x))))))
        (:a :href "index.html" "Back to Index"))))))

(defun chapter-?-?.html ()
  (let ((id-1 (routes.h:route-parameter :id1))
        (id-2 (routes.h:route-parameter :id2)))
    (who:with-html-output-to-string (s)
      (:html
       (:body
        (:h1 (who:fmt "Chapter ~A-~A" id-1 id-2))
        (:p (who:fmt "This is a chapter ~A-~A" id-1 id-2))
        (:a :href (format nil "chapter-~A.html" id-1)
            (who:fmt "Back to Chapter ~A" id-1)))))))

(defparameter *first-vhost* (format nil "archimag:~A" *port*))

(routes.h:connect-handler *map*
                          "routes/chapter-:(id1)-:(id2).html"
                          'chapter-?-?.html
                          :host *first-vhost*
                          :method :get)

(routes.h:connect-handler *map*
                          "routes/chapter-:(id).html"
                          'chapter-?.html
                          :host *first-vhost*
                          :method :get)


(routes.h:connect-handler *map*
                          "routes/index.html"
                          'index.html
                          :host *first-vhost*
                          :method :get)

;;; second vhost

(defparameter *second-vhost* (format nil "tabris:~A" *port*))

(defun welcome-to-tabris.get.html ()
  (who:with-html-output-to-string (s)
    (:html
     (:body
      (:h1 "Welcome!")
      (:form :method "post" :action "index.html"
             (:div "Input test message:")
             (:input :name "message")
             (:input :value "Send" :type "submit"))))))

(defun welcome-to-tabris.post.html ()
  (who:with-html-output-to-string (s)
    (:html
     (:body
      (:h1 "Welcome!")
      (:div (:b "test message: ")
            (who:str (hunchentoot:post-parameter "message")))
      (:a :href "" "Try again")))))

(routes.h:connect-handler *map*
                          "index.html"
                          'welcome-to-tabris.get.html
                          :host *second-vhost*
                          :method :get)

(routes.h:connect-handler *map*
                          "index.html"
                          'welcome-to-tabris.post.html
                          :host *second-vhost*
                          :method :post)
                                    