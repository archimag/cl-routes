;;;; test.lisp
;;;;
;;;; This file is part of the cl-routes library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:routes.test
  (:use #:cl #:lift #:routes #:routes.unify)
  (:export #:run-routes-tests))

(in-package :routes.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routest-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite routes-test () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-template-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite parse-template-test (routes-test) ())

(addtest (parse-template-test)
  parse-template-1
  (ensure-same '("foo" "bar")
               (parse-template "/foo/bar")))

(addtest (parse-template-test)
  parse-template-2
  (ensure-same '("foo" :x "bar" :y)
               (let ((tmpl (parse-template "/foo/:x/bar/:y")))
                 (first tmpl)
                 (template-spec (second tmpl)))))


;; (defparameter *map*)

;; (deftestsuite routes-test ()
;;   (:dynamic-variables *map*)
;;   (:setup (setf *map*
;;                 (make-instance 'mapper))
;;           (connect *map* (make-route ""))
;;           (connect *map* (make-route "foo/bar/" ))
;;           (connect *map* (make-route "data/:(*rest)"))
;;           (connect *map* (make-route "archive/:year/:month/:day"))
;;           (connect *map* (make-route "forum/:chapter/:topic/:message"))
;;           (connect *map* (make-route "archive/:(year)-:(month)-:(day).html"))))

;; (addtest (routes-test)
;;   dynamic-part-1
;;   (ensure-same '((:chapter . "develop") (:topic . "34") (:message . "25"))
;;                (cdr (match map #u"forum/develop/34/25"))))

;; (addtest (routes-test)
;;   grouping-1
;;   (ensure-same '((:year . "2008") (:month . "12") (:day . "12"))
;;                (cdr (match map "archive/2008-12-12.html"))))

;; (addtest (routes-test)
;;   extra-bindings-1
;;   (ensure-same '((:action . feeds) (:chapter . "forum"))
;;                (cdr (match map #u"feeds/forum/atom.xml"))))

;; (addtest (routes-test)
;;   conditions-1
;;   (ensure-same 'develop-comment
;;                (cdr (assoc :view (cdr (match map "forum/develop/453/23/12"))))))

;; (addtest (routes-test)
;;   conditions-2
;;   (ensure-same 'not-develop-comment
;;                (cdr (assoc :view (cdr (match map "forum/live/453/23/12"))))))

;; (addtest (routes-test)
;;   empty-url
;;   (ensure-same "default-handler"
;;                (cdr (assoc :data (cdr (match map ""))))))

;; (addtest (routes-test)
;;   empty-url-2
;;   (ensure-same "default-handler"
;;                (cdr (assoc :data (cdr (match map "/foo/bar/"))))))

;; (addtest (routes-test)
;;   wildcard-1
;;   (ensure-same "rest-handler"
;;                (match map "data/1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-routes-tests ()
  (run-tests :suite 'routes-test :report-pathname nil))
