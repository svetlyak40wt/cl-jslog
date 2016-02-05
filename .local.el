(load-file "lisp/slime-helper.el")
(setq inferior-lisp-program "./run.sbcl")
(slime)

(setq 40wt/tests-result 'unknown)
(setq 40wt/run-testsuite '(cl-jslog-tests:run-tests))


(defun el-testo/log (msg &rest args)
  (message "EL-TESTO: %s" (format msg args)))


(defun 40wt/update-status-line ()
  (setq tests-result-mode-line-string
        (cond
         ((eql 40wt/tests-result 'unknown) "Tests UNKN")
         ((eql 40wt/tests-result 'in-progress) "Tests PROG")
         ((null 40wt/tests-result) "Tests FAIL")
         (t "Tests PASS")))
  (el-testo/log tests-result-mode-line-string)
  (force-mode-line-update))

(defun 40wt/on-slime-eval ()
  (el-testo/log "Form was evaluated")
  (el-testo/log "Now will run tests in repl")

  (setq 40wt/tests-result 'in-progress)
  (setq 40wt/tests-result (slime-eval '(cl-jslog-tests:run-tests)))
  
  (el-testo/log "Evaluation done")
  (40wt/update-status-line)
  (el-testo/log "Tests result is %s" 40wt/tests-result))



;;;###autoload
(define-minor-mode display-tests-result-mode
  "Toggle tests status display in mode line."
  :global t :group 'unittests
  
  (setq tests-result-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  
  (if display-tests-result-mode
      (progn
        (el-testo/log "Activating hooks")
        (add-to-list 'global-mode-string 'tests-result-mode-line-string t)
        (add-hook 'slime-transcript-stop-hook '40wt/on-slime-eval)
        (40wt/update-status-line))
    (progn
      (el-testo/log "Deactivating hooks")
      (setq global-mode-string
            (delq 'tests-result-mode-line-string global-mode-string))
      (remove-hook 'slime-transcript-stop-hook '40wt/on-slime-eval))))

;; (display-tests-result-mode)

