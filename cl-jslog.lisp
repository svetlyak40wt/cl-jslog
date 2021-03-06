(defpackage #:cl-jslog
  (:nicknames #:jslog)
  (:use #:cl
        #:split-sequence
        #+sbcl
        :sb-ext
        #+ecl
        :ext)
  (:export #:main))

(in-package #:cl-jslog)

(proclaim '(optimize (debug 3)))

(defvar +program-name+ "jslog")
(defvar +program-version+ "0.1.0")

(defvar +default-format+
  "(\"[\" @timestamp \"]: \" @fields.level \" \" @message)")

(defvar +default-filter+ "()")
(defvar +example-filter+ "(equal @fields.level \"ERROR\")")


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


(define-condition unparsable-expression (error)
  ((text :initarg :text :reader text)))


(defun parse-lisp-sexps (str)
  (restart-case
      (handler-case (read-from-string str nil)
        (end-of-file ()
          (error (make-condition 'unparsable-expression
                                 :text str))))
    (use-value (new-value)
      :report "Give new expression and try again"
      (parse-lisp-sexps new-value))))


(defun make-filter-from-string (str)

  (let ((rules (parse-lisp-sexps str)))
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
          (when (funcall filter item)
            (process item formatter)))))


(defun get-line-from-stdin ()
  (read-line *standard-input* nil))


(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :version
   :description "print version number and exit"
   :short #\v
   :long "version")
  (:name :filter
   :description (format nil
                        "expression to filter log items. Default: no filter. Example: ~a"
                        +example-filter+)
   :short #\f
   :long "filter"
   :arg-parser #'identity)
  (:name :format
   :description (format nil
                        "expression to format log items. Default: ~a"
                        +default-format+)
   :short #\f
   :long "format"
   :arg-parser #'identity))


(defun print-help ()
  (unix-opts:describe :usage-of +program-name+))


(defun print-version ()
  (format t "~a ~a~%" +program-name+ +program-version+))


(defun output-error-and-exit
    (&optional (error-message "Syntax error in expression \"~A\"~%" ))
  (lambda (c)
   "Handler to handle 'unparsable-expression conditions"
   (format t error-message
           (text c))
   (quit)))


(defun real-main (&optional args)
  (let* ((args (handler-case (opts:get-opts args)
                 (opts:unknown-option (condition)
                   (format t "error: ~s option is unknown!~%"
                           (opts:option condition))
                   (quit))
                
                 (opts:missing-arg (condition)
                   (format t "fatal: option ~s needs an argument!~%"
                           (opts:option condition))
                   (quit))
                
                 (opts:arg-parser-failed (condition)
                   (format t "fatal: cannot parse ~s as argument of ~s~%"
                           (opts:raw-arg condition)
                           (opts:option condition))
                   (quit))))
         (show-help (getf args :help nil))
         (show-version (getf args :version nil))
         
         (format-str (getf args :format +default-format+))
         (formatter (handler-bind ((unparsable-expression
                                    (output-error-and-exit
                                     "Syntax error in format expression \"~A\"~%")))
                      (make-formatter (parse-lisp-sexps format-str))))
         
         (filter-str (getf args :filter +default-filter+))
         (filter (handler-bind ((unparsable-expression
                                 (output-error-and-exit
                                  "Syntax error in filter expression \"~A\"~%")))
                   (make-filter-from-string filter-str))))
    
    ;; (format t "Called with arguments:\"~a\"~%" args)
    ;; (format t "filter-str is:\"~a\"~%" filter-str)
    ;; (format t "filter is:\"~a\"~%" filter)
    ;; (format t "format-str is:\"~a\"~%" format-str)
    ;; (format t "formatter:\"~a\"~%" formatter)

    (cond (show-help (print-help)
                     (exit))
          (show-version (print-version)
                        (exit))
          (t (process-lines #'get-line-from-stdin
                            formatter
                            filter)))))

(defun main ()
  (handler-case (real-main uiop:*command-line-arguments*)
    #+sbcl
    (SB-SYS:INTERACTIVE-INTERRUPT ()
      (progn (format t "~%Exiting because of the interrupt.~%"))))
  (exit))
