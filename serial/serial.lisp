(walk-directory "/home/xhyang/src/lisp-test/practicals-1.0.3/" #'(lambda (pathname)
                                                                   (format t "~a~%" pathname)
                                                                   (if (directory-pathname-p pathname)
                                                                       (setf asdf:*central-registry* (append asdf:*central-registry* (list pathname))))) :directories t)



(setq *serial* (ccl::make-serial-stream "/dev/ttyUSB1" :baud-rate 115200 :char-bits 8 t :stop-bits 1 t :external-format (ccl::make-external-format :line-termination :CRLF)))

(format *serial* "ls~%")

(force-output *serial*)
(read-line *serial*)

(defun read-serial ()
  (format t "~a" (read-line *serial*)))

(loop (read-serial))
