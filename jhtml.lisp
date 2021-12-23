;;;; jhtml.lisp
(in-package #:jhtml)

;;; Variables
(defparameter *special-rules* ())
(defparameter *output-stream* () "The stream to output to. By default returns string.")


;;; Special rules
(defun special-rule-p (symbol)
  (find symbol *special-rules*))

(defmacro define-special-rule (name arglist &body body)
  "Define a rule for a specific type of lists. The rest of the list are used
 as arguments to the function. The defined rule should return a string.

Example:
(define-special-rule doctype (&optional (type \"html\"))
  (format nil \"<!DOCTYPE ~A>\" type))

(jhtml '(doctype)) ;=> \"<!DOCTYPE html>\"
(jhtml '(doctype \"something else\")) ;=> \"<!DOCTYPE something else>\"
"
  `(progn
     (unless (special-rule-p ',name)
       (push ',name *special-rules*))

     (defun ,name ,arglist
       ,@body)))


;;; Void elements (self-enclosing tags)
(defun void-element-definition (name)
  `(define-special-rule ,name (&rest args)
     (format nil "<~A~{ ~A=\"~A\"~}>" (string-downcase ',name) (strip-attributes args))))

(defmacro define-void-elements (&rest elements)
  "Define self-closing tags.

Example:
(define-void-elements img link meta)

Now <img> <link> and <meta> elements won't have the respective </img>, </link>
and </meta> tags."
  `(progn
     ,@ (mapcar #'void-element-definition elements)))


;;; Escapes
(defvar *html-escapes*
  '(#\> "&gt;"
    #\< "&lt;"
    #\& "&amp;"
    #\" "&quot;"))

(defun escape (str)
  (declare (string str))
  (with-output-to-string (stream)
    (loop for ch across str
          for escaped = (getf *html-escapes* ch)
          do (if escaped
                 (write-string escaped stream)
                 (write-char ch stream)))))

;;; List to html
(defun string-value (element)
  (etypecase element
    (null "")
    (number (write-to-string element))
    (string (escape element))
    (cons (transform-tree-element element))))

(defun jhtml-helper (sexp)
  (declare (type cons sexp))
  (multiple-value-bind (attrs sexp) (strip-attributes sexp)
    (when (consp (car sexp))
      (return-from jhtml-helper (format nil "~{~A~}" (mapcar #'string-value sexp))))
    (let ((element (string-downcase (car sexp)))
          (contents (mapcar #'string-value (cdr sexp))))
      (format nil "<~A~{ ~A=\"~A\"~}>~{~A~}</~3:*~A>" element attrs contents))))

(defun strip-attributes (list)
  (do (attrs clean-sexp
       (list list (cdr list)))
      ((null list)
       (values (nreverse attrs) (nreverse clean-sexp)))
    (let ((first (first list)) (second (second list)))
      (if (keywordp first)
          (setf attrs (list* second first attrs)
                list (cdr list))
          (push first clean-sexp)))))

(defun transform-tree-element (list)
  (when (null (car list)) (return-from transform-tree-element ""))
  (let ((special-rule (special-rule-p (car list))))
    (if special-rule
        (apply special-rule (cdr list))
        (jhtml-helper (or list (cdr list))))))

(defun jhtml (&rest lists)
  "Converts `lists' to an HTML string.
The first element of every list has to be a symbol, of any package.
The rest can be any combination of keyword-string pairs, strings or
new lists.

If the first element isn't part of a special rule, it gets treated
like some ordinary html tag name. If it is, the function associated
with the special rule is called and the string it returns is inserted.
See `define-special-rule'.

Example usage:
(jhtml:jhtml
 '(doctype)
 `(html
   (head
    (link :rel \"stylesheet\" :type \"text/css\" :href \"/path/to/styles.css\")
    (title \"My Webpage\"))
   (body
    (h1 \"Heading\")
    (hr)
    (p :class \"article body\"
       ,(server:get-latest-article)))))

Other usage ideas would be to create template functions that return lists,
 which then one would pass to jhtml."
  (format *output-stream* "~{~A~}" (mapcar #'transform-tree-element lists)))

(defun to-string (&rest lists)
  "Output html to string. Useful to avoid unnecessary binding of
`*output-stream*' to nil in contexts where it's bound to a different
value, but the user needs a string, for any reason."
  (let ((*output-stream* nil))
    (apply #'jhtml lists)))
