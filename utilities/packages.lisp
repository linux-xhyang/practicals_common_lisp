;; packages.lisp

(defpackage utilities
  (:use common-lisp iterate split-sequence local-time usocket alexandria)
  (:import-from alexandria once-only with-gensyms if-let)
  (:export newline slurp slurp-stream slurp-lines slurp-line last1 run-to-string
           string-starts-with string-ends-with string-contains-p
           process-string print-table blank-table-line newline
           pad-string
           ))
