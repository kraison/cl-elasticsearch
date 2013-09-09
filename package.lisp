(in-package #:cl-user)

(defpackage #:cl-elasticsearch
  (:use #:cl #:cl-ppcre #:clouchdb #:drakma #:bordeaux-threads)
  (:export #:*es-host*
           #:*es-port*
           #:list-cluster-nodes
           #:node-info
           #:create-index
           #:delete-index
           #:open-index
           #:close-index
           #:refresh-index
           #:optimize-index
           #:add-to-index
           #:update-in-index
           #:delete-from-index
           #:get-from-index
           #:get-settings
           #:update-settings
           #:delete-mapping
           #:get-mapping
           #:add-mapping
           #:es-search
           #:free-form-search))

