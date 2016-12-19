;;; init-windows.el --- my window/frame management
;;; Commentary:
;;; Code:

(use-package split-window
  :init (progn
          (defun split-window-multiple-ways (x y)
            "Split the current frame into a grid of X columns and Y rows."
            (interactive "nColumns: \nnRows: ")
            (delete-other-windows)
            (dotimes (i (1- x))
              (split-window-horizontally)
              (dotimes (j (1- y))
                (split-window-vertically))
              (other-window y))
            (dotimes (j (1- y))
              (split-window-vertically))
            (balance-windows))

          (autoload 'windmove-find-other-window "windmove"
            "Return the window object in direction DIR. fn dir &optional arg window)")

          (declare-function windmove-find-other-window "windmove" (dir &optional arg window))

          (defun get-window-in-frame (x y &optional frame)
            "Find Xth horizontal and Yth vertical window from top-left of FRAME."
            (let ((orig-x x) (orig-y y)
                  (w (frame-first-window frame)))
              (while (and (windowp w) (> x 0))
                (setq w (windmove-find-other-window 'right 1 w)
                      x (1- x)))
              (while (and (windowp w) (> y 0))
                (setq w (windmove-find-other-window 'down 1 w)
                      y (1- y)))
              (unless (windowp w)
                (error "No window at (%d, %d)" orig-x orig-y))
              w))

          (defun set-window-buffer-in-frame (x y buffer &optional frame)
            "Set Xth horizontal and Yth vertical window to BUFFER from top-left of FRAME."
            (set-window-buffer (get-window-in-frame x y frame) buffer))))

(use-package switch-window
  :init (progn
          ;; When splitting window, show (other-buffer) in the new window
          (defun split-window-func-with-other-buffer (split-function)
            (lexical-let ((s-f split-function))
              (lambda ()
                (interactive)
                (funcall s-f)
                (set-window-buffer (next-window) (other-buffer)))))

          (defun split-window-vertically-swap ()
            (interactive)
            (funcall (split-window-func-with-other-buffer 'split-window-vertically)))

          (defun split-window-horizontally-swap ()
            (interactive)
            (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))
  :config (progn
            (setq-default
             switch-window-shortcut-style 'alphabet))
  :ensure t)

(use-package scratch
  :init (progn
          (defun immortal-scratch ()
            (if (eq (current-buffer) (get-buffer "*scratch*"))
                (progn (bury-buffer) nil) t))
          (add-hook 'kill-buffer-query-functions 'immortal-scratch)
          (setq kill-buffer-query-functions
                (remq 'process-kill-buffer-query-function
                      kill-buffer-query-functions))))

(defun prevent-active-processes-exist ()
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    (cl-letf (((symbol-function #'process-list) (lambda ())))
      ad-do-it)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(provide 'init-windows)
;;; init-windows.el ends here
