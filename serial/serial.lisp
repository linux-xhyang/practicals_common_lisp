;; (walk-directory "/home/xhyang/src/lisp-test/practicals-1.0.3/" #'(lambda (pathname)
;;                                                                    (format t "~a~%" pathname)
;;                                                                    (if (directory-pathname-p pathname)
;;                                                                        (setf asdf:*central-registry* (append asdf:*central-registry* (list pathname))))) :directories t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/xhyang/src/ccl/library/serial-streams.lisp"))

(in-package "CL-USER")

(defvar *serial* nil)
(defvar *serial-input-thread* nil)
(defvar *shell-command-list* nil)
(defvar *serial-log* nil)



(setq *serial* (ccl::make-serial-stream "/dev/ttyUSB3" :baud-rate 115200
                                                       :char-bits 8 t
                                        :stop-bits 1 t :termios.c_lflag #$ECHO
                                                       :external-format (ccl::make-external-format :line-termination :CRLF)))

(defun string-in-list (str)
  (loop for i in *list-string*
        do
           (when (search i str :test #'char-equal)
             (return str))))

(defun read-serial (serial save-stream)
  (let ((str (read-line serial)))
    (format save-stream "~a~%" str)
    str))

(defun serial-log-collect (serial)
  (let* ((save-file (format nil "/home/xhyang/logs-~a.txt" (get-universal-time)))
         (save-stream (open save-file :direction :output :if-does-not-exist :create)))
    (loop while t
          do
             (let ((str (read-serial serial save-stream)))
               (when (string-in-list str)
                 (setq *serial-log* (cons str *serial-log*))
                 (print *serial-log*)
                 )))))

(defun serial-log-get (timeout)
  (loop while t
        do
           (let ((str (pop *serial-log*)))
             (if str
               (return str)
             (cond
               ((> timeout 0)
                (progn
                  (sleep 1)
                  (setq timeout (- timeout 1))))
               ((= timeout 0))
               ((< timeout 0)
                (sleep 1))))
             )))

(defun serial-force-output (serial str)
  (format serial str)
  (format t "serial force output:~a~%" str)
  (format serial "~%")
  (force-output serial)
  (sleep 1))

(defun wait-shell (serial comm result fail-result timeout repeat)
  (loop while t
        do
           (let ((str (serial-log-get timeout)))
             (if (and  (> repeat 0) fail-result (search fail-result str :test #'char-equal))
                 (progn
                   (if comm
                       (progn
                         (serial-force-output serial comm)
                         (if (> 0 repeat)
                             (setq repeat (- repeat 1)))
                         )))
                 (when (search result str :test #'char-equal)
                   (return str))))))

(defun shell-callback (serial result fail-result)
  (format t "Machine shell start~%")
  (serial-force-output serial  "su")
  (sleep 1)
  (wait-shell serial "su" result fail-result 5 5))

(defun power-callback (serial result fail-result)
  (format t "Machine shut down~%"))

;;  (setq *serial-input-thread*  (process-run-function "Input enter" 'input-loop *serial*))
(defun startup-callback (serial result fail-result)
  (format t "Machine start up~%"))

(defun wdt-detect (serial result fail-result)
  (format t "~a ~%" result ))

(defun bootfinish-callback (serial result fail-result)
  (serial-force-output serial "getprop | grep dev.bootcomplete")
  (sleep 1)
  (wait-shell serial "getprop | grep dev.bootcomplete" result fail-result -1 5)
  (format t "Now do reboot machine~%")
  (serial-force-output serial "svc power reboot")
  (sleep 1)
  (setq *serial-log* nil))

;; (when (search "wdt_reset = "
;;                 (wait-shell serial nil "wdt_reset = " fail-result)  :test #'char-equal)
;;     (format t "wdt reset ~%")
;;     )

(defvar *list-command*
  (list
   (cons (list "Power down" "Power down" nil) 'power-callback)
   (cons (list "Booting Linux" "Booting Linux" nil) 'startup-callback)
   (cons (list "wdt_reset = "  "wdt_reset = " nil) 'wdt-detect)
   (cons (list "shell@gladiator:/ $" "shell@gladiator:/ #" nil)  'shell-callback)
   (cons (list "shell@gladiator:/ #" "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #") 'bootfinish-callback)
   ))

(defvar *list-string*
  (list "Power down" "Booting Linux" "wdt_reset = " "shell@gladiator:/ $"
        "shell@gladiator:/ #" "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #"))

;; (defun command-list (str)
;;   (if (> (length str) 0)
;;       (let ((default-list *list-command*))
;;         (loop for command in *list-command*
;;               do
;;                  (when (search (car (car command)) str :test #'char-equal)
;;                    (return default-list))
;;                  (setq default-list (cdr default-list))
;;               ))))

(defun serial-loop (serial)
  (let ((str (serial-log-get -1)))
    (mapcar #'(lambda (list)
                    (let ((key (car  (car list)))
                          (result (cadr (car list)))
                          (fail-result (caddr (car list)))
                          (func (cdr list)))
                      (when (search key str :test #'char-equal)
                        (format t "serial loop command")
                        (print list)
                        (format t "~%")
                        (funcall func serial result fail-result)))) *list-command*)))

(progn
  (setq *serial-log* nil)
  (setq *serial-input-thread*  (process-run-function "Input Collect"
                                                     'serial-log-collect *serial*))
  (serial-force-output *serial* "ls init.rc")
  (loop (serial-loop *serial*)))
