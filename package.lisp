;;;; package.lisp

(defpackage #:jhtml
  (:use #:cl)
  (:export #:jhtml
           #:define-special-rule
           #:define-void-elements
           #:*output-stream*
           ;; pre-defined special rules
           #:doctype
           #:insert-html))

(in-package #:jhtml)
