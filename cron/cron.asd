(defpackage :cron-system (:use :asdf :cl))
(in-package :cron-system)

(defsystem cron-job
  :name "cron-job"
  :author "xhyang <linux.xhyang@gmail.com>"
  :version "1.0"
  :maintainer "xhyang <linux.xhyang@gmail.com>"
  :licence "BSD"
  :description "Code For Cron-job"
  :long-description ""
  :depends-on (:cl-cron ::external-program)
  :components
  ((:file "packages")
   (:file "cron-system" :depends-on ("packages"))))
