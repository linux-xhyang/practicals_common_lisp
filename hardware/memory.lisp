;; memory.lisp

(in-package :hardware)

(defun memory-information ()
  (print-table (process-string (slurp-lines "/proc/meminfo") '((:split #\:) (:trim #\space)))
               :right-justified t))

(defclass pagetype-migrate ()
  ((node
    :initarg :node
    :initform 0
    :accessor pagetype-node)
   (zone
    :initarg :zone
    :initform nil
    :reader pagetype-zone)
   (migrate
    :initarg :migrate
    :initform nil
    :accessor pagetype-migrate)
   (buddy
    :initarg :buddy
    :initform nil
    :accessor pagetype-buddy)
   (total
    :initarg :total
    :accessor pagetype-total))
  (:documentation "/proc/pagetypeinfo"))

(defun expt-2-list (list)
  (let ((count 0)
        (result nil))
    (iter (for var in list)
      (setf result (append result (list (* var (expt 2 count)))))
      (incf count)
      )
    result))

(defun compute-total (list)
  (let ((total 0))
    (iter (for var in list)
      (setq total (+ total var))
      )
    total))

(defun make-pagetype-migrate-entry (node zone migrate buddy)
  (make-instance 'pagetype-migrate
                 :node node
                 :zone zone
                 :migrate migrate
                 :buddy buddy
                 :total (compute-total buddy))
  )

(defun parse-pagetype-info (file)
  (let ((header nil)
        (free-pages nil))
    (with-open-file (input file :external-format (detect-external-format-from-file file :cn))
      (iter (for line in-stream input using #'read-line)
        (cond
          ((string-starts-with line "Page block order:")
           (format t "order ~a~%" (cadr (process-string line '((:split #\:) (:trim #\space)))))
           t)
          ((string-starts-with line "Pages per block:")
           (format t "page per block ~a~%" (cadr (process-string line '((:split #\:) (:trim #\space) ))))
           t)
          ((string-starts-with line "Free pages count per migrate type at order")
           (format t "start parse migrate~%")
           t)
          ((string-starts-with line "Number of blocks type")
           (format t "start parse block~%")
           (setq header t)
           t)
          ((string-starts-with line "Node ")
           (if header
               (progn

                 (format t "header ~a~a~%" header line)
                 )
               (progn
                 (let* ((level0 (process-string line '((:split #\,) (:trim #\space))))
                        (node (read-from-string (cadr (process-string (car level0) '((:split #\space) )))))
                        (zone (cadr (process-string (cadr level0) '((:split #\space)))))
                        (migrate (cadr (process-string (caddr level0) '((:split #\space)))))
                        (buddy (expt-2-list (mapcar #'(lambda (x)
                                                        (read-from-string x)) (cddr (process-string (caddr level0) '((:split #\space) (:trim #\return))))))))
                                        ;(format t "~a " node)
                                        ;(format t "~a " zone)
                                        ;(format t "~a " migrate)
                                        ;(format t "~a ~%" buddy)
                   (push (make-pagetype-migrate-entry node zone migrate buddy) free-pages)
                   )))
           t)
          )
        ))
    free-pages))

(defun pagetype-zone-summary (pagetypeinfo zone)
  (let ((total nil))
    (iter (for page in pagetypeinfo)
      (when (string-starts-with (pagetype-zone page) zone)
        (if total
            (setf total (mapcar #'+ total (pagetype-buddy page)))
            (setf total (pagetype-buddy page)))
        )
      )
    total))

(defun fragment (total list)
  (/ (- total (compute-total list))
     (* 1.0 total)))

(defun pagetype-fragment-summary(buddy)
  (let* ((total (compute-total buddy))
         (count 0)
         (list nil))
    (iter (for var in buddy)
      (setf list (append list (list (fragment total (nthcdr count buddy)))))
      (incf count)
      )
    list)
  )

(defun pagetype-parse (file &optional (page_size 4096) (manual t))
  (let* ((filename (if file
                       file
                       "/proc/pagetypeinfo"))
         (pagetypeinfo (parse-pagetype-info filename))
         (total (pagetype-free-total pagetypeinfo page_size manual))
         (dma (pagetype-zone-summary pagetypeinfo "DMA"))
         (normal (pagetype-zone-summary pagetypeinfo "Normal"))
         (highmem (pagetype-zone-summary pagetypeinfo "HighMem"))
         (dma-frag (pagetype-fragment-summary dma))
         (normal-frag (pagetype-fragment-summary normal))
         (highmem-frag (pagetype-fragment-summary highmem))
         )
    (print total)
    (format t "~%DMA:~%" )
    (print dma)
    (format t "~%Normal:~%" )
    (print normal)
    (format t "~%HighMem:~%" )
    (print highmem)
    (format t "~%DMA Fragment:~%" )
    (print dma-frag)
    (format t "~%Normal Fragment:~%" )
    (print normal-frag)
    (format t "~%HighMem Fragment:~%" )
    (print highmem-frag)
    t)
  )

(defun pagetype-free-total (pagetypeinfo &optional (page_size 4096) (manual t))
  (let* ((div (if manual
                  (/ 1048576.0 page_size)
                  1))
         (total 0)
         (dma 0)
         (normal 0)
         (highmem 0)
         (unmovable 0)
         (reclaim 0)
         (movable 0)
         (reserve 0)
         (cma 0)
         (isolate 0))
    (iter (for page in pagetypeinfo)
      (setf total (+ total (pagetype-total page)))
      (let ((zone (pagetype-zone page))
            (migrate (pagetype-migrate page)))
        (cond
          ((string-contains-p zone "DMA")
           (setf dma (+ dma (pagetype-total page)))
           t)
          ((string-contains-p zone "Normal")
           (setf normal (+ normal (pagetype-total page)))
           t)
          ((string-contains-p zone "HighMem")
           (setf highmem (+ highmem (pagetype-total page)))
           t)
          )
        (cond
          ((string-contains-p migrate "Unmovable")
           (setf unmovable (+ unmovable (pagetype-total page)))
           t)
          ((string-contains-p migrate "Reclaimable")
           (setf reclaim (+ reclaim (pagetype-total page)))
           t)
          ((string-contains-p migrate "Movable")
           (setf movable (+ movable (pagetype-total page)))
           t)
          ((string-contains-p migrate "Reserve")
           (setf reserve (+ reserve (pagetype-total page)))
           t)
          ((string-contains-p migrate "CMA")
           (setf cma (+ cma (pagetype-total page)))
           t)
          ((string-contains-p migrate "Isolate")
           (setf isolate (+ isolate (pagetype-total page)))
           t))
        )
      )
    (list :total (/ total div) :dma (/ dma div) :normal (/ normal div) :highmem (/ highmem div)
          :unmoveable (/ unmovable div) :reclaim (/ reclaim div) :movable (/ movable div)
          :reserve (/ reserve div) :cma (/ cma div) :isolate (/ isolate div)) ))

(defun pagetypeinfo ()
  (iter (for line in-file "/proc/pagetypeinfo" using #'read-line)
    (when (string-starts-with line "Node ")
      (process-string line )

      )
    ))
