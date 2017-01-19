;;; tasks.el --- my task management system
;;; Commentary:
;;; Code:

;; TASKS I'm working on
(defun keys (inlist)
  "Return keys of INLIST."
  (let ((alkeys (list)))
    (mapc (lambda (key) (add-to-list 'alkeys (car key))) inlist)
    alkeys))

(setq-default balance/current-tasks '("BALANCE-24291 — stat_aggregator"
                                      "BALANCE-24258 — release"))

(setq-default balance/actions
              '(("*browse*" . (lambda (task) (browse-url
                                         (concat "https://st.yandex-team.ru/"
                                                 (car (split-string task))))))
                ("*insert*" . (lambda (task) (insert (car (split-string task)))))))

(defun balance/process-task ()
  "Insert one of my current tasks name."
  (interactive)
  (let ((balance/current-task (ido-completing-read "Task: " balance/current-tasks))
        (balance/process-method (ido-completing-read "Method: " (keys balance/actions))))
    (funcall (cdr (assoc balance/process-method balance/actions)) balance/current-task)))
(global-set-key (kbd "C-x y b") 'balance/process-task)

(provide 'tasks)
;;; tasks.el ends here
