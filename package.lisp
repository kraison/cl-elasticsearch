(in-package #:cl-user)

(defpackage #:cl-elasticsearch
  (:use #:cl #:cl-ppcre #:clouchdb #:drakma #:bordeaux-threads)
  (:export #:*es-host*
           #:*es-port*
           #:list-nodes
           #:node-info
           #:create-index
           #:delete-index
           #:refresh-index
           #:optimize-index
           #:add-to-index
           #:delete-from-index
           #:get-from-index
           #:delete-mapping
           #:get-mapping
           #:add-mapping
           #:es-search))

