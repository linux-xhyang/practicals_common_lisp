;; utilities.asd

(defsystem :utilities
  :serial t
  :components ((:static-file "utilities.asd")
               (:file "packages")
               (:file "first")
               (:file "print-table")
               (:file "slurping")
               (:file "strings"))
  :depends-on (:iterate :split-sequence :local-time :swank :usocket
                :fare-utils :cl-ppcre :lift :alexandria))
