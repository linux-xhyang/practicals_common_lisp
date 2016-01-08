(in-package :cron-system)

(defparameter *build-command* "ls")

(defun read-all-from-stream (stream)
  "Read characters from a stream into a string until EOF."
  (concatenate 'string
               (loop for byte = (read-char-no-hang stream nil nil)
                     while byte collecting byte)))

(defun list-directory-stream (directory)
  ;; This stream is closed by bizarre iter:in-stream later anyway
  (external-program:process-output-stream
   (external-program:start "ls" (list (namestring (truename directory))) :output :stream)))

(defun list-directory-filenames (directory)
  (iter (for line in-stream (list-directory-stream directory) using #'read-line)
        (collect line)))

(defun with-output-stream (command &key args)
  (let* ((proc (external-program:start command args :input :stream
                                                    :output :stream))
         (stream (external-program:process-output-stream proc)))
    (handler-case
        (progn
          (iter (for line in-stream stream using #'read-line)
            (collect line))
          )
      (simple-error (c)
        (declare (ignore c))
        (format t "~D" c)
        (external-program:signal-process proc :quit)))
    ))

(defmacro with-output-line ((line command &key args) &body body)
  `(let* ((proc (external-program:start ,command ,args :input :stream
                                                    :output :stream))
         (stream (external-program:process-output-stream proc)))
    (handler-case
        (progn
          (iter (for line in-stream stream using #'read-line)
            (setq ,line line)
            ,@body)
          )
      (simple-error (c)
        (declare (ignore c))
        (external-program:signal-process proc :quit)))
    ))

(defparameter *project-list* nil
  )

(defparameter *build-log* "/build-log.txt")
(defparameter *build-job* nil)

(defun save-job (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *project-list* out))))

(defun load-job (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *project-list* (read in)))))

(defun build-system ()
  (mapcar #'(lambda (project)
              (let* ((name (car project))
                     (dir (caadr project))
                     (scripts (cadadr project))
                     (log (format nil "~a/~a" dir *build-log*))
                     )
                (with-open-file (save log :direction :output
                                          :if-does-not-exist :create
                                          :if-exists :supersede)
                  (cl-cwd:with-cwd  dir
                    (with-output-line (line "bash" :args (list scripts))
                      (with-standard-io-syntax
                        (print line save))))
                  )))
          *project-list*))

(defun start-cron-job()
  (load-job "~/note/todo/jobs.l")
  (let ((build (cl-cron:make-cron-job #'build-system :hour 1 :minute 30)))
    (push build *build-job*)
    (cl-cron:start-cron)
    ))

(defun stop-cron-job()
  (cl-cron:stop-cron)
  (loop for job in *build-job*
        do
           (cl-cron:delete-cron-job job)
           (pop *build-job*)
        )
  (save-job "~/note/todo/jobs.l")
  )
