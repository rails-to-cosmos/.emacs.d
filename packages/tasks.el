;;; tasks.el --- my task management system
;;; Commentary:
;;; Code:

;; TASKS I'm working on
;; (defun keys (inlist)
;;   "Return keys of INLIST."
;;   (let ((alkeys (list)))
;;     (mapc (lambda (key) (add-to-list 'alkeys (car key))) inlist)
;;     alkeys))

;; (setq-default balance/current-tasks
;;               '("TASKGROUP-24291 — task name"))

;; (defun task-manager/add-task (&key task desc rel) (list task desc rel)
;;        (message task))

;; (defun task-manager/browse-task (task-name)
;;   (browse-url
;;    (concat "https://st.yandex-team.ru/"
;;            (car (split-string task-name)))))

;; (setq-default balance/actions
;;               '(("browse" . task-manager/browse-task)
;;                 ("insert" . (lambda (task) (insert (car (split-string task)))))))

;; (defun balance/process-task ()
;;   "Insert one of my current tasks name."
;;   (interactive)
;;   (let ((balance/current-task (ido-completing-read "Task: " balance/current-tasks))
;;         (balance/process-method (ido-completing-read "Method: " (keys balance/actions))))
;;     (funcall (cdr (assoc balance/process-method balance/actions)) balance/current-task)))
;; (global-set-key (kbd "C-x y b") 'balance/process-task)

(provide 'tasks)
;;; tasks.el ends here
