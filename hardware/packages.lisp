;; packages.lisp

(defpackage hardware
  (:use common-lisp iterate split-sequence local-time usocket alexandria utilities cffi)
  (:export memory-information
           cpus cpu-limits
           )
  )
