(in-package :cl-elasticsearch)

(defun es-request (query-string &key args content (method :get)
                   (host *es-host*) (port *es-port*))
  (let ((uri (format nil "http://~A:~A/~A" host port query-string)))
    (log:debug "ES URI: ~A" uri)
    (let ((*drakma-default-external-format* :utf-8))
      (multiple-value-bind (stream status headers)
          (http-request uri
                        :method method
                        :want-stream t
                        :parameters args
                        :content content
                        :content-length t)
        (unwind-protect
             (progn
               (log:debug "~A~%~A" status headers)
               (cond ((= status 204)
                      t)
                     ((and (>= status 200) (<= status 299))
                      (let ((response (clouchdb::decode-json stream)))
                        ;;(log:debug "~A" response)
                        response))
                     ((= status 404)
                      (let ((response (clouchdb::decode-json stream)))
                        (log:debug "ES ERROR RESPONSE: ~A" response)
                        (error 'unknown-resource-error
                               :message
                               (format nil "~A" response))))
                     (t
                      (let ((response (clouchdb::decode-json stream)))
                        (log:debug "ES ERROR RESPONSE: ~A" response)
                        (error 'elasticsearch-error
                               :message
                               (format nil "~A: ~A" status response))))))
          (when (open-stream-p stream) (close stream)))))))

(defun list-cluster-nodes ()
  (es-request "_cluster/nodes/_local"))

(defun node-info (name-or-ip)
  (es-request (format nil "_cluster/nodes/~A" name-or-ip)))

(defun create-index (name)
  (es-request name :method :put))

(defun open-index (name)
  (es-request (format nil "~A/_open" name) :method :post))

(defun close-index (name)
  (es-request (format nil "~A/_close" name) :method :post))

(defun delete-index (name)
  (es-request name :method :delete))

(defun refresh-index (name)
  (es-request (format nil "~A/_refresh" name) :method :post))

(defun optimize-index (name)
  (es-request (format nil "~A/_optimize" name) :method :post))

(defun add-to-index (id item type index-name &key parent)
  (let ((args nil))
    (when parent
      (push `("parent" . ,(format nil "~A" parent)) args))
    (es-request (format nil "~A/~A/~A" index-name type id)
                :method :put
                :args args
                :content (document-to-json item))))

(defun delete-from-index (id type index-name)
  (es-request (format nil "~A/~A/~A" index-name type id) :method :delete))

(defun get-from-index (id type index-name &key fields)
  (let ((args nil))
    (when fields
      (push `("fields" . ,(format nil "~{~A~^,~}" fields)) args))
    (es-request (format nil "~A/~A/~A" index-name type id)
                :args args
                :method :get)))

(defun get-settings (index-name)
  (es-request (format nil "~A/_settings" index-name) :method :get))

(defun update-settings (settings index-name)
  ;;(close-index index-name)
  ;;(prog1
  (es-request (format nil "~A/" index-name)
              :method :put
              :content settings))
  ;;(open-index index-name)))

(defun delete-mapping (type index-name)
  (es-request (format nil "~A/~A/_mapping" index-name type) :method :delete))

(defun get-mapping (type index-name)
  (es-request (format nil "~A/~A/_mapping" index-name type) :method :get))

(defun encode-mapping (mapping type)
  (with-output-to-string (stream)
    (json:with-object (stream)
      (json:as-object-member (type stream)
        (json:with-object (stream)
          (json:as-object-member ("properties" stream)
            (json:with-object (stream)
              (map nil
                   #'(lambda (def)
                       (json:as-object-member
                           ((symbol-name (first def)) stream)
                         (json:with-object (stream)
                           (map nil
                                #'(lambda (slot)
                                    (json:encode-object-member
                                     (string-downcase
                                      (symbol-name (car slot)))
                                     (cdr slot)
                                     stream))
                                (second def)))))
                   mapping))))))))

(defun add-mapping (mapping type index-name)
  (let* ((map (with-output-to-string (stream)
                (json:with-object (stream)
                  (json:as-object-member (type stream)
                    (json:with-object (stream)
                      (json:as-object-member ("properties" stream)
                        (json:with-object (stream)
                          (map nil
                               #'(lambda (def)
                                   (json:as-object-member
                                       ((symbol-name (first def)) stream)
                                     (json:with-object (stream)
                                       (map nil
                                            #'(lambda (slot)
                                                (json:encode-object-member
                                                 (string-downcase
                                                  (symbol-name (car slot)))
                                                 (cdr slot)
                                                 stream))
                                            (second def)))))
                               mapping))))))))
         (r (es-request (format nil "~A/~A/_mapping" index-name type)
                        :method :PUT
                        :content map)))
    r))

(defun build-filter (filter stream)
  (json:with-object (stream)
    (dolist (pair filter)
      (cond ((null pair) nil)
            ((atom (cdr pair))
             (json:encode-object-member (car pair) (cdr pair) stream))
            ((consp (cdr pair))
             (json:as-object-member ((car pair) stream)
               (build-filter (second pair) stream)))))))

(defun build-query (query stream)
  (json:with-object (stream)
    (if query
        (json:as-object-member ("match" stream)
          (json:with-object (stream)
            (map nil #'(lambda (i)
                         (json:encode-object-member
                          (symbol-name (car i)) (cdr i) stream))
                 query)))
        (json:as-object-member ("match_all" stream)
          (json:with-object (stream))))))

(defun build-sort (sort stream)
  (map nil
       #'(lambda (s)
           (cond
             ((symbolp s)
              (json:encode-array-member
               (symbol-name s) stream))
             ((stringp s)
              (json:encode-array-member s stream))
             ((and (consp s) (symbolp (car s)) (atom (cdr s)))
              (json:as-array-member (stream)
                (json:with-object (stream)
                  (json:encode-object-member
                   (symbol-name (car s)) (cdr s) stream))))
             ((and (consp s) (stringp (car s)) (atom (cdr s)))
              (json:as-array-member (stream)
                (json:with-object (stream)
                  (json:encode-object-member
                   (car s) (cdr s) stream))))
             ((and (consp s) (consp (cdr s)))
              (json:as-array-member (stream)
                (build-filter (list s) stream)))
             ))
       sort))

(defun build-search-query (query &key sort filter facets fields no-fields? size from)
  (declare (ignore facets))
  (with-output-to-string (stream)
    (json:with-object (stream)
      (when sort
        (json:as-object-member ("sort" stream)
          (json:with-array (stream)
            (build-sort sort stream))))
      (cond (no-fields?
             (json:as-object-member ("fields" stream)
               (json:with-array (stream))))
            (fields
             (json:as-object-member ("fields" stream)
               (json:with-array (stream)
                 (dolist (field fields)
                   (json:encode-array-member
                    (if (symbolp field)
                        (symbol-name field)
                        field)
                    stream))))))
      (if filter
          (json:as-object-member ("query" stream)
            (json:with-object (stream)
              (json:as-object-member ("filtered" stream)
                (json:with-object (stream)
                  (json:as-object-member ("query" stream)
                    (build-query query stream))
                  (json:as-object-member ("filter" stream)
                    (build-filter filter stream))))))
          (json:as-object-member ("query" stream)
            (build-query query stream))))))

(defun es-search (query type index-name &key sort filter fields no-fields?
                  return-ids? facets size from)
  (let ((q (build-search-query query
                               :sort sort
                               :fields fields
                               :filter filter
                               :facets facets
                               :size size
                               :from from
                               :no-fields? (or no-fields? return-ids?))))
    (format t "~A~%" q)
    (let ((parameters nil))
      (when size
        (push `("size" . ,(format nil "~D" size)) parameters))
      (when from
        (push `("from" . ,(format nil "~D" from)) parameters))
      (let ((r (es-request (format nil "~A/~A/_search" index-name type)
                           :method :get
                           :args parameters
                           :content q)))
        (when (and (@ r :|hits|) (@ (@ r :|hits|) :|total|)
                   (> (@ (@ r :|hits|) :|total|) 0))
          (if return-ids?
              (mapcar #'(lambda (hit)
                          (@ hit :|_id|))
                      (@ (@ r :|hits|) :|hits|))
              (@ (@ r :|hits|) :|hits|)))))))

(defun free-form-search (string type index-name &key size from)
  (let ((parameters nil))
    (when size
      (push `("size" . ,(format nil "~D" size)) parameters))
    (when from
      (push `("from" . ,(format nil "~D" from)) parameters))
    (let ((r (es-request (format nil "~A/~A/_search" index-name type)
                         :method :get
                         :args parameters
                         :content string)))
      (when (and (@ r :|hits|) (@ (@ r :|hits|) :|total|)
                 (> (@ (@ r :|hits|) :|total|) 0))
        (@ (@ r :|hits|) :|hits|)))))
