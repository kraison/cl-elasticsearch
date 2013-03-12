(in-package :cl-elasticsearch)

;;; The following code was borrowd from cl-couch is subject to copyright:
;; Copyright (C) 2008
;; Ryszard Szopa <ryszard.szopa@gmail.com> & Nick Allen <nallen05@gmail.com>
(defun alist-plist (alist)
  "Transform an alist into a plist."
  (iterate:iter (iterate:for (k . v) iterate:in alist)
        (unless (and (null k) (null v))
          (iterate:appending (list k v)))))

(defun plist-alist (plist)
  "Transform a plist into an alist."
  (iterate:iter (iterate:for k iterate:in plist iterate::by 'cddr)
                (iterate:for v iterate:in (cdr plist) iterate::by 'cddr)
                (iterate:collect (cons k v))))

(defun plist-alist* (&rest rest)
  (plist-alist rest))

(defgeneric aget (key alistish)
  (:documentation "Do a `key' lookup in `alistish' and return two
  values: the actual value and whether the lookup was successful (just
  like `gethash' does)."))

(defmethod aget (key (alist list))
  "The value of `key' in `alist'"
  (let ((x (assoc key alist :test #'equal)))
    (values
     (cdr x)
     (consp x))))

(defmethod aget (key (hash hash-table))
  (gethash key hash))

(defmethod (setf aget) (value key (hash hash-table))
  (setf (gethash key hash) value))

(defmethod (setf aget) (value key (alist list))
  (if (null (assoc key alist))
      (progn
        (rplacd alist (copy-alist alist))
        (rplaca alist (cons key value))
        value)
      (cdr (rplacd (assoc key alist) value))))

(defun @ (alist key &rest more-keys)
  "Swiss army knife function for alists and any objects for whom
  `aget' has been defined. It works on alistish objects much much like
  the dot `.' works in many other object oriented
  languages (eg. Python, JavaScript).

  (@ alistish-object :foo :bar :baz) is equivalent to
  calling (aget :baz (aget :bar (aget :foo x))), or
  alistish_object.foo.bar.baz in JS. A setter for `@' is also
  defined.

  It returns two values: the actual value and whether the lookup was
  successful (just like `gethash' does).

  `equal' is used for testing identity for keys."
  (if (null more-keys)
      (aget key alist)
      (apply '@ (aget key alist) more-keys)))

(defun (setf @) (val alist key &rest more-keys)
  (if (null more-keys)
      (setf (aget key alist) val)
      (setf (apply #'@ (aget key alist) more-keys) val)))

(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

(proclaim '(inline ensure-list))
;;; End borrowed code
