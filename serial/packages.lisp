(in-package :cl-user)

(defpackage :serial-test
  (:use
   :common-lisp
   :iterate
   :alexandria
   :cl-cron
   :external-program
   )
  (:export :serial-test
           ))
