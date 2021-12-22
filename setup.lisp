(in-package #:jhtml)

(define-special-rule doctype (&optional (type "html"))
  (format nil "<!DOCTYPE ~A>" type))

(define-special-rule insert-html (&rest html)
  (apply #'concatenate (list* 'string html)))

;; void elements
(define-void-elements :area :base :br :col :embed :hr :img :link :meta :param :source :track :wbr)
