(in-package #:jhtml)

(define-special-rule doctype (&optional (type "html"))
  (format nil "<!DOCTYPE ~A>" type))

(define-void-elements area base br col embed hr img link meta param source track wbr)
