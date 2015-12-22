(defpackage :serial-test (:use :asdf :cl))
(in-package :serial-test)

(defsystem serial-test
  :name "serial-test"
  :author "xhyang <linux.xhyang@gmail.com>"
  :version "1.0"
  :maintainer "xhyang <linux.xhyang@gmail.com>"
  :licence "BSD"
  :description "Code For Serial Test"
  :long-description ""
  :depends-on (:cl-cwd
               :iterate
               :alexandria
               :cl-cron
               :external-program)
  :components
  ((:file "packages")
   (:file "serial" :depends-on ("packages"))))
