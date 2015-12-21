(in-package :cron-system)

(defparameter *build-command* "ls")

(defmacro with-build-stream ((stream command &key args) &body body)
  (let ((proc (gensym)))
    `(let* ((,proc (external-program:start ,command ,args :input :stream :output :stream))
	    (,stream (external-program:process-output-stream ,proc)))
       (handler-case
	   (progn ,@body
		  (format ,stream "~%quit~%")
		  (finish-output ,stream))
	 (simple-error (c)
	   (declare (ignore c))
	   (external-program:signal-process ,proc :quit)))
       ,proc)))

(defun read-all-from-stream (stream)
  "Read characters from a stream into a string until EOF."
  (concatenate 'string
               (loop for byte = (read-char-no-hang stream nil nil)
                     while byte collecting byte)))

      ;; (progn
      ;;   (flet ((process-stdout ()
      ;;            (read-all-from-stream
      ;;             out)))
      ;;     (let ((output (process-stdout)))
      ;;       (loop until (search  output) do
      ;;         (let ((new-output (process-stdout)))
      ;;           (setf output (concatenate 'string output new-output))))))
      ;;   )

(defun test()
  (external-program:run "ls" (list "-l"))
  (format t "Hello World ~%"))

(defun start-test()
  (cl-cron:make-cron-job #'test :boot-only t)
  (cl-cron:make-cron-job #'test :minute 1)
  (cl-cron:start-cron))
