(in-package #:jhtml)

(define-special-rule doctype (&optional (type "html"))
  (format nil "<!DOCTYPE ~A>" type))

(defun handle-raw (element)
  (etypecase element
    (null "")
    (number (write-to-string element))
    (string element)))
(define-special-rule insert-html (&rest html)
  (setf html (mapcar #'handle-raw html))
  (apply #'concatenate (list* 'string html)))

;; void elements
(define-void-elements :area :base :br :col :embed :hr :img :link :meta :param :source :track :wbr)
