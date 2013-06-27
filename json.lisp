(in-package :json)

(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
     for code = (char-code ch)
     with special
     if (setq special (car (rassoc ch +json-lisp-escaped-chars+)))
       do (write-char #\\ stream) (write-char special stream)
     else
       do (write-char ch stream)))


(in-package :clouchdb)

(defun write-json-chars (s stream)
  (declare (inline lisp-special-char-to-json))
  (loop for ch across s
     for code = (char-code ch)
     for special = (lisp-special-char-to-json ch)
     do
     (cond
       ((and special (not (char= special #\/)))
        (write-char #\\ stream)
        (write-char special stream))
       ;;((<= code #x1f)
       ;; (format stream "\\u~4,'0x" code))
       (t (write-char ch stream)))))
