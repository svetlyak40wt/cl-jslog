(defpackage #:cl-jslog
  (:nicknames #:jslog)
  (:use #:cl #:split-sequence)
  (:export #:main))

(in-package #:cl-jslog)

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

;(funcall (make-formatter '("[" @timestamp "]: " @fields.level " " @message)) *parsed*)

(defun process (line formatter)
  (format t "~a~%" (funcall formatter line)))


(defun process-lines (get-next-line)
  (let* ((format '("[" @timestamp "]: " @fields.level " " @message))
         (formatter (make-formatter format)))
    (loop
       as line = (funcall get-next-line)
       while line
       do (let ((item (parse line)))
            (process item formatter)))))


(defun get-line-from-stdin ()
  (read-line *standard-input* nil))


(defun main ()
  (process-lines #'get-line-from-stdin))
