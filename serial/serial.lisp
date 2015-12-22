;; (walk-directory "/home/xhyang/src/lisp-test/practicals-1.0.3/" #'(lambda (pathname)
;;                                                                    (format t "~a~%" pathname)
;;                                                                    (if (directory-pathname-p pathname)
;;                                                                        (setf asdf:*central-registry* (append asdf:*central-registry* (list pathname))))) :directories t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/xhyang/src/ccl/library/serial-streams.lisp"))

(in-package "CL-USER")

(defvar *serial* nil)
(defvar *serial-input-thread* nil)
(defvar *serial-main-thread* nil)
(defvar *shell-command-list* nil)
(defvar *serial-log* nil)
(defvar *loop-count* nil)
(defvar *wdt-reset* nil)

(defvar *key-list* (list (cons "Home" 3)
                         (cons "Back" 4)
                         (cons "down" 20)
                         (cons "up" 19)
                         (cons "left" 21)
                         (cons "right" 22)
                         (cons "enter" 66)
                         (cons "sound+" 24)
                         (cons "sound-" 25)
                         (cons "power" 66)))

(defvar *activity-list* (list
                         (list "HDPPlayer" (cons "hdpfans.com/hdp.player.StartActivity"
                               "hdpfans.com"))
                         (list "desktop" (cons "com.xiaomi.tv.desktop/.ui.MainActivity"
                               "com.xiaomi.tv.desktop"))
                         (list "tvplayer" (cons "com.xiaomi.mitv.tvplayer/.AtvActivity"
                               "com.xiaomi.mitv.tvplayer"))
                         ))

(setq *activity-list* (list
                         (list "HDPPlayer" (cons "hdpfans.com/hdp.player.StartActivity"
                               "hdpfans.com"))
                         (list "desktop" (cons "com.xiaomi.tv.desktop/.ui.MainActivity"
                               "com.xiaomi.tv.desktop"))
                         (list "tvplayer" (cons "com.xiaomi.mitv.tvplayer/.AtvActivity"
                               "com.xiaomi.mitv.tvplayer"))
                         ))

(defvar *list-command*
  (list
   (cons (list "Power down" "Power down" nil) 'power-callback)
   (cons (list "<< MStar >>#" "<< MStar >>#" nil) 'mboot-callback)
   (cons (list "Booting Linux" "Booting Linux" nil) 'startup-callback)
   (cons (list "wdt_reset = "  "wdt_reset = 0" "wdt_reset = 1") 'wdt-detect)
   (cons (list "shell@gladiator:/ " "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #") 'bootfinish-callback)
   ))

(defvar *list-string*
  (list "Error: Activity" "* TaskRecord" "<< MStar >>#" "Emergency Remount" "Restarting system" "Power down" "Booting Linux" "wdt_reset = " "shell@gladiator:/ $"
         "shell@gladiator:/ #" "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #"))

(setq *list-string* (list "Error: Activity" "* TaskRecord" "<< MStar >>#" "Emergency Remount" "Restarting system" "Power down" "Booting Linux" "wdt_reset = " "shell@gladiator:/ $"
         "shell@gladiator:/ #" "[dev.bootcomplete]: [1]" "1|shell@gladiator:/ #"))

(defvar *filter-string*
  (list "dumpsys" "su" "getprop" "grep" "svc power" "cpudvfsscaling"))

(setq *filter-string* (list "dumpsys" "su" "getprop" "grep" "svc power" "cpudvfsscaling"))

(setq *loop-count* 0)
(setq *serial* (ccl::make-serial-stream "/dev/ttyUSB3"
                                        :baud-rate 115200
                                        :char-bits 8 t
                                        :flow-control :none
                                        :stop-bits 1 t
                                        :external-format (ccl::make-external-format :line-termination :CRLF)))

(defvar *test-case* (list
                     ;;app-name (key list) period repeat
                     (list "HDPPlayer"
                           (list "up" "down") 100 6)
                     (list "tvplayer"
                           (list "up" "down" "right" "enter") 100 6)
                     (list "desktop"
                           (list "up" "down" "right" "enter" "back" "home") 10 10)
                     ))
(setq *test-case* (list
                     ;;app-name (key list) period repeat
                     (list "HDPPlayer"
                           (list "up" "down") 100 6)
                     (list "tvplayer"
                           (list "up" "down" "right" "enter") 100 6)
                     (list "desktop"
                           (list "up" "down" "right" "enter" "back" "home") 10 10)
                     ))

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

(defun serial-log-search (key &key timeout)
  (block log-search
    (loop for entry in *serial-log*
          do
          (when (search key entry :test #'char-equal)
            (return-from log-search entry))
          (when timeout
            (setq timeout (- timeout 1))
            (sleep 1)
            (if (<= timeout 0)
                (return-from log-search nil))
            )
          )))

(defun serial-force-output (serial str)
  (format serial str)
  (force-output serial)
  (sleep 0.01)
  (format serial "~%")
  (force-output serial)
  ;;(format t "serial output ~a~%" str)
  (sleep 1))

(defun serial-interrupt (serial)
  (stream-write-char serial (code-char 3))
  (sleep 0.1)
  (force-output serial)
  (sleep 1)
  (serial-force-output serial "ls /init.rc")
  )

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
  (format t "run command: ~a ~%" comm)
  (when (wait-shell serial "" 10)
    (loop while (>= repeat 0)
          do
             (serial-log-clear)
             (let ((r (serial-run-program serial comm result fail-result)))
               (if r
                   (progn
                     (sleep 1)
                     (serial-log-clear)
                     (return r)))
               (setq repeat (- repeat 1)))
          )))

(defun serial-run-wait (serial comm result fail-result &key error collect timeout)
  ;;success return result
  ;;failed will rerun the command
  ;;error handle used work error happen,ensure need redo comm
  (format t "run command: ~a ~%" comm)
  (when (wait-shell serial "" 10)
    (serial-log-clear)
    (serial-force-output serial comm)
    (sleep 2)
    (loop while t
          do
             (let ((s (serial-log-search result :timeout timeout))
                   (f (if fail-result (serial-log-search fail-result) nil))
                   (cont t))
               (if (and f fail-result)
                   (progn
                     (when error
                       (setq cont (funcall error f)))
                     (if cont
                         (progn
                           (format t "failed,run retry~%")
                           (serial-interrupt serial)
                           (wait-shell serial "" 10)
                           (serial-log-clear)
                           (serial-force-output serial comm)
                           (sleep 2))
                         (progn
                           (serial-log-clear)
                           (return f)))
                     )
                   (if (or s timeout)
                       (progn
                         (if s
                             (progn
                               (format t "wait ~a result ~a success~%" comm
                                       result)
                               (when collect
                                 (setq s (funcall collect *serial-log*)))
                               (serial-log-clear)
                               (return s))
                             (progn
                               (format t "timeout,run retry~%")
                               (serial-interrupt serial)
                               (wait-shell serial "" 10)
                               (serial-log-clear)
                               (serial-force-output serial comm)
                               (sleep 2))
                         ))))
               ))))

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
    (format t "wdt reset found~%"))
  (wait-shell serial "" 300))

(defun shutdown-machine (serial)
  (format t "Now do reboot machine~%")
  (serial-run-wait serial "svc power shutdown" "" "shell@gladiator:/ #")
  )

(defun search-key-by-name (key)
  (loop for entry in *key-list*
        do
           (when (search key (car entry) :test #'char-equal)
             (return entry))))

(defun input-key-event (serial name)
  (let* ((key (search-key-by-name name))
         (comm nil))
    (when key
      (setq comm (format nil "input keyevent ~D" (cdr key)))
      (format t "input key ~a ~%" name)
      (serial-run-wait serial comm "shell@gladiator:/ #" nil)
      )))

(defun top-activity (serial)
  (let* ((stack nil)
        (result (serial-run-wait serial
                                 "dumpsys activity activities |busybox grep \"* TaskRecord\""
                                 "shell@gladiator:/ " nil
                                 :collect #'(lambda (log)
                                              (mapcar #'(lambda (entry)
                                                          (when (search "* TaskRecord" entry :test #'char-equal)
                                                            (push entry stack))) log)
                                              ) :timeout 60)))
    (when result
      (pop stack))))

(defun start-activity (serial name result repeat)
  (loop while (> repeat 0)
        do
           (let* ((comm (format nil "am start -n ~a" name))
                 (ret (serial-run-wait serial comm "shell@gladiator:/ "
                              "Error: Activity" :error #'(lambda (err)
                                                    (when (search "Error: Activity" err :test #'char-equal)
                                                      (format t "Error: Activity~%")
                                                      nil
                                                      )) :timeout 60)))
             (if (search "Error: Activity" ret :test #'char-equal )
                 (progn
                   (format t "~a not found,please check~%" name)
                   (return nil))
                 (progn
                   (sleep 5)
                   (when (search result (top-activity serial) :test #'char-equal)
                     (return result))
                   (format t "run app ~a [~a] failed~%" name result)
                   (setq repeat (- repeat 1)))
                 ))
        ))

(defun serial-run-app (serial name result)
  ;;nil run app failed
  ;;name, run app name
  (let ((current (top-activity serial)))
    (if (search name current :test #'char-equal)
        (progn
          (format t "~a is running on~%" name)
          name)
        (progn
          (when (start-activity serial name result 5)
            name
            )))
    ))

(defun find-app (name)
  (loop for app in *activity-list*
        do
           (when (search name (car app) :test #'char-equal)
             (return (cadr app)))))

(defun run-app (serial name)
  (let ((app (find-app name)))
    (when app
        (serial-run-app serial (car app) (cdr app)))
    )
  )

(defun get-random-from-list (list)
  (let* ((state (make-random-state t))
         (n (random (length list) state)))
    (nth n list)
    ))

(defun random-number (n)
  (let ((state (make-random-state t)))
    (+ 1 (random n state))))

(defun run-test-case (serial)
  (let* ((case (get-random-from-list *test-case*))
         (app (car case))
         (key-list (cadr case))
         (period (caddr case))
         (repeat (random-number (cadddr case))))
    (format t "run test case ~a period ~D repeat ~D~%" app period repeat)
    (when (run-app serial app)
      (loop repeat repeat
            do
               (sleep (random-number period))
               (input-key-event serial (get-random-from-list key-list))
               (sleep (random-number period))
            ))))

(defun start-serial-test (serial)
  (format t "Start Test~%")
  (loop repeat 2
        do
           (run-test-case serial))
  (sleep 10)
  (input-key-event serial "Home")
  (shutdown-machine serial)
  )

(defun wdt-save-log (serial)
  (let* ((save-file (format nil "/data/logs-~a"
                            (get-universal-time)))
         (prepare (format nil "mkdir -p ~a" save-file))
         (comm (format nil "busybox cp /data/log ~a -rf" save-file)))
    (serial-run serial prepare "shell@gladiator:/ #" "1|shell@gladiator:/ " 10)
    (serial-run serial comm "shell@gladiator:/ #" "1|shell@gladiator:/ " 10)
    )
  )

(defun bootfinish-callback (serial str result fail-result)
  (serial-run serial "su" "shell@gladiator:/ #" "shell@gladiator:/ $" 60)
  (serial-run serial "getprop | grep dev.bootcomplete" result fail-result 60)

  (when  *wdt-reset*
    (wdt-save-log serial)
    (setq *wdt-reset* nil)
    )
  (start-serial-test serial)
  )

(defun mboot-callback (serial str result fail-result)
  (serial-force-output serial "reset"))

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

(defun serial-test-main-thread ()
  (progn
    (setq *serial-log* nil)
    (setq *serial-input-thread*  (process-run-function "Input Collect"
                                                       'serial-log-collect
                                                       *serial*))
    (sleep 1)
    (wait-shell *serial* "" 60)
    (loop (serial-loop *serial*))))


(defun serial-kill-thread()
  (let ((process-list (all-processes)))
    (when *serial-input-thread*
      (process-kill *serial-input-thread*)
      (setq *serial-input-thread* nil))
    (when *serial-main-thread*
      (process-kill *serial-main-thread*)
      (setq *serial-main-thread* nil))
    (loop for process in process-list
          do
             (when (search "Input Collect" (process-name process) :test #'char-equal)
               (process-kill process))
       )))

(defun serial-test()
  (serial-kill-thread)
  (setq *serial-main-thread* (process-run-function "Input Clollect Main"
                                                 'serial-test-main-thread)))

;;;;;TEST
;;(serial-force-output *serial* "")
;;(top-activity *serial*)
