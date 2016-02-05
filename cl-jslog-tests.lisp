(defpackage #:cl-jslog-tests
  (:use
   #:common-lisp
   #:alexandria
   #:should-test)
  (:export #:run-tests))

(in-package #:cl-jslog-tests)

(defvar *synthetic-timestamp* 0)

(defun make-item (level message)
  (incf *synthetic-timestamp*)
  
  (alist-hash-table
   `(("@timestamp" . *synthetic-timestamp*)
     ("@message" . ,message)
     ("@fields" . ,(alist-hash-table
                    `(("level" . ,level))
                    :test #'equal)))
   :test #'equal))

(deftest test-parsing ()
  (let* ((line "{\"one\": 1}")
         (parsed (jslog::parse line)))
    
    (should be hash-table-p parsed)
    (should be = 1 (gethash "one" parsed))))


(deftest test-value-access ()
  (let* ((line "{\"one\": {\"two\": 1}")
         (parsed (cl-jslog::parse line)))
    
    (should be =
            1
            (cl-jslog::get-field-value 'one.two parsed))
    (should be equal
            "Address"
            (cl-jslog::get-field-value "Address" parsed))))


(deftest test-formatter ()
  (let* ((formatter
          (jslog::make-formatter
           '("[" @timestamp "]: " @fields.level " " @message)))
         (item (alist-hash-table
                `(("@timestamp" . 1)
                  ("@message" . "Initialization")
                  ("@fields" . ,(alist-hash-table
                                 '(("level" . "INFO"))
                                 :test #'equal)))
                :test #'equal))
         (result (funcall formatter item)))
    
    (should be equal "[1]: INFO Initialization" result)))


(deftest test-filter ()
  (let* ((flt
          (jslog::make-filter
           '(equal @fields.level "INFO"))))
    
    (should be eql
            t
            (funcall flt (make-item "INFO" "msg")))
    (should be eql
            nil
            (funcall flt (make-item "ERROR" "msg")))))

;; (deftest test-complex-filter ()
;; 
;;   (let* ((flt
;;           (jslog::make-filter
;;            '(and (equal @fields.level "ERROR")
;;                  (= @number 123 )))))
    
;;     (should be eql
;;             t
;;             (funcall flt (make-item "INFO" "msg")))
;;     (should be eql
;;             nil
;;             (funcall flt (make-item "ERROR" "msg")))))



(deftest test-get-function ()
  (should be null
          (cl-jslog::get-function "blah"))
  (should be null
          (cl-jslog::get-function 'minor))
  (should be functionp
          (cl-jslog::get-function 'list)))

(defun run-tests (&key (failed nil))
  (let ((should-test:*test-output* *standard-output*))
    (st:test :failed failed
             :package 'cl-jslog-tests)))

; (should-test:test)


;; (defun check-stdout ()
;;   (format t "Some text"))

;; (check-stdout)
