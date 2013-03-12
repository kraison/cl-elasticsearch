;; ASDF package description for cl-elasticsearch              -*- Lisp -*-

(defpackage :cl-elasticsearch-system (:use :cl :asdf))
(in-package :cl-elasticsearch-system)

(defsystem cl-elasticsearch
  :name "cl-elasticsearch"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Elasticsearch API client."
  :depends-on (:cl-ppcre
	       :clouchdb
               :cl-json
               :iterate
               :drakma
               :log4cl
               :bordeaux-threads)
  :components ((:file "package")
               (:file "globals" :depends-on ("package"))
               (:file "utilities" :depends-on ("globals"))
               (:file "conditions" :depends-on ("utilities"))
               (:file "client" :depends-on ("conditions"))
               (:file "api" :depends-on ("client"))))
