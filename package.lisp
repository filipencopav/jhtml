;;;; package.lisp

(defpackage #:jhtml
  (:use #:cl)
  (:export #:jhtml
           #:define-special-rule
           #:define-void-elements
           #:*output-stream*))

(in-package #:jhtml)
