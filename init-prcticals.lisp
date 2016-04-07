
(in-package :cl-user)

(export '(practicals-pathname))

(defparameter *practicals-path*
  (make-pathname :directory (pathname-directory (or *load-truename*
                                                   *compile-file-truename*))))

(defun practicals-pathname (&key directory name type)
  (merge-pathnames (make-pathname :directory (cons :relative directory)
                                  :name name :type type)
                   *practicals-path*))

(asdf:initialize-source-registry `(:source-registry
                                   (:tree ,(practicals-pathname))
                                   :inherit-configuration))

(format t "~&~%* Initializing Practicals.")
(format t "~%  The Practicals path is: ~a" (directory-namestring *practicals-path*))

;;(maphash #'(lambda (k v) (print (list k v))) asdf/source-registry:*source-registry*)

;;(asdf:operate 'asdf:load-op :id3v2 :practicals t)
