(in-package :cl-elasticsearch)

(define-condition elasticsearch-error (error)
  ((message :accessor message :initarg :message))
  (:report (lambda (condition stream)
             (with-slots (message) condition
               (format stream "Elasticsearch error: ~A" message)))))

(define-condition unknown-resource-error (error)
  ((message :accessor message :initarg :message))
  (:report (lambda (condition stream)
             (with-slots (message) condition
               (format stream "Unknown resource error: ~A" message)))))
