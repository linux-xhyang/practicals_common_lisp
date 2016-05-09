;; hardware.asd

(defsystem :hardware
  :serial t
  :components ((:static-file "hardware.asd")
               (:file "packages")
               (:file "memory")
               (:file "cpu")
               ;;(:file "wireless")
               )
  :depends-on (:utilities :split-sequence :cl-fad :anaphora :cffi :iterate
               :alexandria :inquisitor))
