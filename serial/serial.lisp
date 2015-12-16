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
(defvar *loop-count* nil)
(defvar *wdt-reset* nil)

(setq *loop-count* 0)
(setq *serial* (ccl::make-serial-stream "/dev/ttyUSB3"
                                        :baud-rate 115200
                                        :char-bits 8 t
                                        :flow-control :none
                                        :stop-bits 1 t
                                        :external-format (ccl::make-external-format :line-termination :CRLF)))

(defun string-in-list (str)
  (loop for i in *list-string*
        do
           (when (search i str :test #'char-equal)
             (return str))))

(defun string-in-filter (str)
  (loop for i in *filter-string*
        do
           (when (search i str :test #'char-equal)
             (return str))))

(defun read-serial (serial save-stream)
  (let ((str (read-line serial)))
    (format save-stream "~a~%" str)
    (force-output save-stream)
    str))

(defun serial-log-collect (serial)
  (let* ((save-file (format nil "/home/xhyang/logs-~a.txt" (get-universal-time)))
         (save-stream (open save-file :direction :output :if-does-not-exist :create)))
    (loop while t
          do
             (let ((str (read-serial serial save-stream)))
               (when (and (string-in-list str) (not (string-in-filter str)))
                 ;;                 (setq *serial-log* (cons str *serial-log*))
                 (push str *serial-log*)
                 (format t "#######")
                 (print *serial-log*)
                 (format t "~%")
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
                   ((= timeout 0) (return nil))
                   ((< timeout 0)
                    (sleep 1))))
             )))

(defun serial-log-clear()
  (setq *serial-log* nil))

(defun serial-log-search (key)
  (block log-search
    (loop for entry in *serial-log*
          do
          (when (search key entry :test #'char-equal)
            (return-from log-search entry))
          )))

(defun serial-force-output (serial str)
  (format serial str)
  (force-output serial)
  (sleep 0.01)
  (format serial "~%")
  (force-output serial)
  (format t "serial output ~a~%" str)
  (sleep 1))

(defun wait-shell (serial comm timeout)
  (loop while (>= timeout 0)
        do
           (let ((str (serial-log-search "shell@gladiator:/ ")))
             (if str
                 (progn
                   (format t "wait shell success~%")
                   (sleep 1)
                   (return str))
                 (progn
                   (setq timeout (- timeout 1))
                   (serial-force-output serial comm)
                   (sleep 1))
                 ))))

(defun wait-result (serial result fail-result timeout)
  (loop while (>= timeout 0)
        do
           (let ((s (serial-log-search result))
                 (f (serial-log-search fail-result)))
             (if f
                 (progn
                   (format t "failed~%")
                   (return nil))
                 (if s
                     (progn
                       (format t "wait ~a success~%" result)
                       (return s))))
             (sleep 1)
             (setq timeout (- timeout 1))
             )))

(defun serial-run-program (serial comm result fail-result)
  (block run
  (if (wait-shell serial "" 5)
      (progn
        (serial-log-clear)
        (serial-force-output serial comm)
        (wait-result serial result fail-result 5))
      (progn
        (format t "no shell found~%")
        (return-from run nil)))))

(defun serial-run (serial comm result fail-result repeat)
  (loop while (>= repeat 0)
        do
           (serial-log-clear)
           (let ((r (serial-run-program serial comm result fail-result)))
             (if r
                 (progn
                   (serial-log-clear)
                   (return r)))
             (setq repeat (- repeat 1)))
        ))

(defun serial-run-wait (serial comm result fail-result)
  (when (wait-shell serial "" 10)
    (serial-log-clear)
    (serial-force-output serial comm)
    (sleep 2)
    (loop while t
          do
             (let ((s (serial-log-search result))
                   (f (serial-log-search fail-result)))
               (if f
                   (progn
                     (format t "failed,run second~%")
                     (serial-log-clear)
                     (serial-force-output serial comm)
                     (sleep 2))
                   (if s
                       (progn
                         (format t "wait ~a success~%" result)
                         (return s))))
               )
          )))

(defun power-callback (serial str result fail-result)
  (format t "Machine shut down~%"))

;;  (setq *serial-input-thread*  (process-run-function "Input enter" 'input-loop *serial*))
(defun startup-callback (serial str result fail-result)
  (setq *loop-count* (+ *loop-count* 1))
  (format t "Machine start up ~a~%" *loop-count*)
  (wait-shell serial "" 100))

(defun wdt-detect (serial str result fail-result)
  (when (search fail-result str :test #'char-equal)
    (setq *wdt-reset* t)
    (format t "wdt reset found~%")))

(defun bootfinish-callback (serial str result fail-result)
  (format t "Machine shell start~%")
  (serial-run serial "su" "shell@gladiator:/ #" "shell@gladiator:/ $" 60)
  (serial-run serial "getprop | grep dev.bootcomplete" result fail-result 60)
  (format t "Now do reboot machine~%")
  (serial-run-wait serial "svc power shutdown" "" "shell@gladiator:/ #"))

(defun mboot-callback (serial str result fail-result)
  (serial-force-output serial "reset"))

(defvar *list-command*
  (list
   (cons (list "Power down" "Power down" nil) 'power-callback)
   (cons (list "<< MStar >>#" "<< MStar >>#" nil) 'mboot-callback)
   (cons (list "Booting Linux" "Booting Linux" nil) 'startup-callback)
   (cons (list "wdt_reset = "  "wdt_reset = 0" "wdt_reset = 1") 'wdt-detect)
   (cons (list "shell@gladiator:/ " "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #") 'bootfinish-callback)
   ))

;; (setq *list-command* (list
;;    (cons (list "Power down" "Power down" nil) 'power-callback)
;;    (cons (list "<< MStar >>#" "<< MStar >>#" nil) 'mboot-callback)
;;    (cons (list "Booting Linux" "Booting Linux" nil) 'startup-callback)
;;    (cons (list "wdt_reset = "  "wdt_reset = 0" "wdt_reset = 1") 'wdt-detect)
;;    (cons (list "shell@gladiator:/ " "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #") 'bootfinish-callback)
;;    ))
(defvar *list-string*
  (list "<< MStar >>#" "Emergency Remount" "Restarting system" "Power down" "Booting Linux" "wdt_reset = " "shell@gladiator:/ $"
         "shell@gladiator:/ #" "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #"))

;; (setq *list-string* (list "<< MStar >>#" "Emergency Remount" "Restarting system" "Power down" "Booting Linux" "wdt_reset = " "shell@gladiator:/ $"
;;          "shell@gladiator:/ #" "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #"))

(defvar *filter-string*
  (list "su" "getprop" "grep" "svc power"))

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
                    (funcall func serial str result fail-result)))) *list-command*)))

(defun kill-input-thread()
  (let ((process-list (all-processes)))
    (loop for process in process-list
          do
             (when (search "Input Collect" (process-name process))
               (process-kill process))
       )))

(progn
  (kill-input-thread)
  (setq *serial-log* nil)
  (setq *serial-input-thread*  (process-run-function "Input Collect"
                                                     'serial-log-collect
                                                     *serial*))
  (sleep 1)
  (wait-shell *serial* "" 60)
  (loop (serial-loop *serial*)))
