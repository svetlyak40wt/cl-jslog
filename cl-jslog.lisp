(defpackage #:cl-jslog
  (:nicknames #:jslog)
  (:use #:cl #:split-sequence)
  (:export #:main))

(in-package #:cl-jslog)

(proclaim '(optimize (debug 3)))


(defvar *default-format*
  "(\"[\" @timestamp \"]: \" @fields.level @message)")

(defvar *default-filter* "()")


;(defvar *line* "{\"@fields\": {\"uuid\": \"d9e4d994-9807-455f-873d-863b2ee5ce94\", \"level\": \"INFO\", \"status_code\": 200, \"content_type\": \"text/html; charset=utf-8\", \"path\": \"/\", \"method\": \"GET\", \"name\": \"django.http\"}, \"@timestamp\": \"2016-01-29T09:10:31+00:00\", \"@source_host\": \"abb31f44d577\", \"@message\": \"Request processed\"}")

(defun parse (line)
  (jonathan:parse line :as :hash-table))


(defun get-field-value (rule obj)
  "Extracts data form a hash-table like this:
   
   (get-field-value '@fields.logger.name obj) -> \"ERROR\"

   if rule is not a symbol, then returns it is as is:

   (get-field-value \"Address\" obj) -> \"Address\"
"
  (if (symbolp rule)
      (let* ((asstring (string-downcase (symbol-name rule)))
             (field-names (split-sequence #\. asstring)))
    
        (labels ((drill-down (names obj)
                   (cond ((null names)
                          obj)
                         (t (drill-down
                             (cdr names)
                             (gethash (first names)
                                      obj))))))
          (drill-down field-names obj)))
      ;; otherwise, just return rule as is
      rule))


(defun apply-rule (s rule obj)
  (cond ((stringp rule)
         (format s rule))
        ((symbolp rule)
         (format s "~a" (get-field-value rule obj)))))


(defun make-formatter (rules)
  (lambda (obj)
    (with-output-to-string (s)
      (mapc (lambda (rule)
              (apply-rule s rule obj))
            rules))
    ))


(defun get-function (var)
  (when (and (symbolp var)
             (fboundp var))
    (symbol-function var)))


(defun make-filter (rules)
  (if (get-function (first rules))
      (lambda (obj)
        (let* ((func (get-function (first rules)))
               (args (mapcar (lambda (field)
                               (get-field-value field obj))
                             (cdr rules))))
          (apply func args)))))


(defun format-cond-message (e)
  (format nil "Error ~a occured~%" e))


(define-condition unparsable-filter (error)
  ((text :initarg :text :reader text)))


(defun make-filter-from-string (str)

  (let ((rules (handler-case (read-from-string str nil)
                 (end-of-file ()
                   (error (make-condition 'unparsable-filter
                                          :text str))))))
    (if rules
        (make-filter rules)
        (lambda (item)
          (declare (ignore item))
          t))))

;(funcall (make-formatter '("[" @timestamp "]: " @fields.level " " @message)) *parsed*)

(defun process (line formatter)
  (format t "~a~%" (funcall formatter line)))


(defun process-lines (get-next-line formatter filter)
  (loop
     as line = (funcall get-next-line)
     while line
     do (let ((item (parse line)))
          (process item formatter))))


(defun get-line-from-stdin ()
  (read-line *standard-input* nil))


(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :filter
   :description "expression to filter log items"
   :short #\f
   :long "filter"
   :arg-parser #'identity)
  (:name :format
   :description "expression to format log items"
   :short #\f
   :long "format"
   :arg-parser #'identity))


(defun real-main (&optional args)
  (let* ((args (handler-case (opts:get-opts args)
                 (opts:unknown-option (condition)
                   (format t "warning: ~s option is unknown!~%" (opts:option condition)))
                
                 (opts:missing-arg (condition)
                   (format t "fatal: option ~s needs an argument!~%"
                           (opts:option condition)))
                
                 (opts:arg-parser-failed (condition)
                   (format t "fatal: cannot parse ~s as argument of ~s~%"
                           (opts:raw-arg condition)
                           (opts:option condition)))))
         (format-str (getf args :format *default-format*))
         (filter-str (getf args :filter *default-filter*)))
    
    (format t "Called with arguments:\"~a\"~%" args)
    (process-lines #'get-line-from-stdin
                   (make-filter (read-from-string filter-str))
                   (make-formatter (read-from-string format-str)))))

(defun main ()
  (real-main uiop:*command-line-arguments*))
