;;; parse.lisp

(in-package :routes)

;;; make-variable-symbol

(defun make-variable-symbol (name)
  (intern (string-upcase name)))

;;; parse-path

(defun parse-path (str)
  (if (> (length str) 0)
      (let ((pos (position #\: str)))
        (if pos
            (let ((rest (if (char= (elt str (1+ pos)) #\{)
                            (let ((end (position #\} str :start (1+ pos))))
                              (if end
                                  (cons (make-unify-template 'variable
                                                             (make-variable-symbol (subseq str (+ pos 2) end)))
                                        (handler-case
                                            (parse-path (subseq str (1+ end)))
                                          (condition () (error "Bad format of the ~S" str))))
                                  (error "Bad format of the ~S" str)))
                            (list (make-unify-template 'variable
                                                       (make-variable-symbol (subseq str (1+ pos))))))))
              (if (> pos 0)
                  (cons (subseq str 0 pos) rest)
                  rest))
            (list str)))))

;;; parse-template

(defun parse-template (uri)
  (iter (for path in (split-sequence #\/ uri :remove-empty-subseqs t))
        (collect (let ((spec (parse-path path)))
                   (if (cdr spec)
                       (make-unify-template 'unify::concat spec)
                       (car spec))))))

