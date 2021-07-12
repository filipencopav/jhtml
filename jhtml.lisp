;;;; jhtml.lisp

(in-package #:jhtml)

(defvar *void-elements*
  '(area
    base
    br
    col
    embed
    hr
    img
    link
    meta
    param
    source
    track
    wbr)
  "*VOID-ELEMENTS* is a list of symbols.
Contains the self-terminating elements. If the HTML tag is one of these,
an closing tag, such as </p>, won't be added.")

(defvar *special-rules*
  '((doctype . "<!DOCTYPE html>"))
  "*SPECIAL-RULES* is a list of dotted pairs, CAR being a symbol and CDR being a string.
A dotted pair shall contain a symbol and a string to use in this case.")

(defun symbol= (x y)
  (when (and (symbolp x) (symbolp y))
    (string= (symbol-name x)
             (symbol-name y))))

(defun transform-tree-element (elem)
  (let ((special-rule (find elem *special-rules* :test #'symbol= :key #'car)))
    (if special-rule
        (cdr special-rule)
        (jhtml-helper elem))))

(defun jhtml (&rest s-expressions)
  (format nil "~{~A~}" (mapcar #'transform-tree-element s-expressions)))

(defun strip-attributes (list)
  (do ((attrs ()) (clean-sexp ()) (cons list))
      ((null cons) (values (nreverse attrs) (nreverse clean-sexp)))
    (let ((car (car cons)) (cadr (cadr cons)))
      (if (keywordp car)
          (progn
            (push (list car cadr) attrs)
            (setf cons (cdr cons)))
          (push car clean-sexp))
      (setf cons (cdr cons)))))

(defun string-value (element)
  (the (or string cons) element)
  (etypecase element
    (string element)
    (cons (jhtml-helper element))))

(defun jhtml-helper (sexp)
  (the cons sexp)
  (the symbol (car sexp))
  (multiple-value-bind (attrs sexp) (strip-attributes sexp)
    (format nil "<~A~{ ~{~A=\"~A\"~}~}~:[>~{~A~}</~A>~; />~]"
            (string-downcase (string (car sexp)))
            attrs
            (find (car sexp) *void-elements* :test #'symbol=)
            (mapcar #'string-value (cdr sexp))
            (string-downcase (string (car sexp))))))
