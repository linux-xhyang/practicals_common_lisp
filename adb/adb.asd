(defpackage :adb-system (:use :asdf :cl))
(in-package :adb-system)

(defsystem adb
  :name "adb"
  :author "xhyang <linux.xhyang@gmail.com>"
  :version "1.0"
  :maintainer "xhyang <linux.xhyang@gmail.com>"
  :licence "BSD"
  :description "Code For adb"
  :long-description ""
  :depends-on (:cl-cwd
               :iterate
               :alexandria
               :external-program)
  :components
  ((:file "packages")
   (:file "adb" :depends-on ("packages"))))
