(in-package :adb-system)

(defmacro run-command-with-output-stream ((stream command &key args) &body body)
  `(let* ((proc (external-program:start ,command ,args :input :stream :output
                                                           :stream
                                                           ))
         (,stream (external-program:process-output-stream proc)))
    (flet ((check-status (proc)
             (multiple-value-bind (status exit-code) (external-program:process-status proc)
               (unless (and (eq status :exited) (zerop exit-code))
                 (error "Running \"ping ~a\" produced exit status ~s, code ~s" (car ,args) status exit-code)
                 )
               (format t "Running \"ping ~a\" produced exit status ~s, code ~s" (car ,args) status exit-code)
               )
             proc))
      ,@body
      (check-status proc)
      )
    ))

(run-command-with-output-stream (stream "ping" :args (list "-i 1" "-c 4"
                                                           "192.168.31.109"))
                                (let ((line ""))
                                  (iter (for line in-stream stream using #'read-line)
                                    (format t "~a~%" line)))
                                )
