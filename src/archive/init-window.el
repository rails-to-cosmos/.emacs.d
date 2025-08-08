;; (use-package ace-window
;;     :config (setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h ?i)
;;                   aw-scope 'frame)
;;     :bind (("C-x C-o" . ace-window)
;;            ;; ("C-x C-o" . other-frame)
;;            ("M-o" . ace-window)))

;; (defun my-window< (wnd1 wnd2)
;;   "Return true if WND1 is less than WND2.
;; This is determined by their respective window coordinates.
;; Windows are numbered top down, left to right."
;;   (let* ((f1 (window-frame wnd1))
;;          (f2 (window-frame wnd2))
;;          (e1 (window-edges wnd1))
;;          (e2 (window-edges wnd2))
;;          (p1 (frame-position f1))
;;          (p2 (frame-position f2))
;;          (nl (or (null (car p1)) (null (car p2)))))
;;     (cond ((and (not nl) (< (car p1) (car p2)))
;;            (not aw-reverse-frame-list))
;;           ((and (not nl) (> (car p1) (car p2)))
;;            aw-reverse-frame-list)
;;           ((< (car e1) (car e2))
;;            t)
;;           ((> (car e1) (car e2))
;;            nil)
;;           ((< (cadr e1) (cadr e2))
;;            t))))

;; (defun my-window-list ()
;;   "Return the list of interesting windows."
;;   (sort
;;    (cl-remove-if
;;     (lambda (w)
;;       (let ((f (window-frame w)))
;;         (or (not (and (frame-live-p f)
;;                       (frame-visible-p f)))
;;             (string= "initial_terminal" (terminal-name f))
;;             (aw-ignored-p w))))
;;     (cl-case aw-scope
;;       (visible
;;        (cl-mapcan #'window-list (visible-frame-list)))
;;       (global
;;        (cl-mapcan #'window-list (frame-list)))
;;       (frame
;;        (window-list))
;;       (t
;;        (error "Invalid `aw-scope': %S" aw-scope))))
;;    'my-window<))

;; (defun my-switch-to-window (window)
;;   "Switch to the window WINDOW."
;;   (let ((frame (window-frame window)))
;;     (when (and (frame-live-p frame)
;;                (not (eq frame (selected-frame))))
;;       (select-frame-set-input-focus frame))
;;     (if (window-live-p window)
;;         (select-window window)
;;       (error "Got a dead window %S" window))))

;; (let ((windows (cl-loop for window in (my-window-list)
;; 		  for window-buffer = (window-buffer window)
;; 		  if (not (eq window-buffer (current-buffer)))
;; 		  collect (list (buffer-name window-buffer) window))))
;;   (if (= 1 (length windows))
;;       (my-switch-to-window (cadar windows))
;;     (mapcar #'my-switch-to-window (alist-get (completing-read "Window: " windows) windows nil nil #'string=))))

(define-minor-mode pin-mode
    "Make the current window always display this buffer."
  nil
  #(" " 0 1 (rear-nonsticky t display nil font-lock-face (:family "github-octicons" :height 1.2) face (:family "github-octicons" :height 1.2)))
  nil
  (let ((window (selected-window)))
    (cond (pin-mode (set-window-dedicated-p window pin-mode)
                    (set-window-parameter window 'no-other-window t)
                    (set-window-parameter window 'no-delete-other-windows t))
          (t (set-window-dedicated-p window pin-mode)
             (set-window-parameter window 'no-other-window nil)
             (set-window-parameter window 'no-delete-other-windows nil)))))

(global-set-key [f11] 'pin-mode)

(define-temporary-mode shrink-window-mode
    "Navigate between git hunks in the buffer."
  :lighter  " Win"
  :install  "C-x -"
  :bindings (("-" . shrink-window-horizontally)
             ("=" . enlarge-window-horizontally)))

(define-temporary-mode enlarge-window-mode
    "Navigate between git hunks in the buffer."
  :lighter  " Win"
  :install  "C-x +"
  :bindings (("-" . shrink-window-horizontally)
             ("=" . enlarge-window-horizontally)))

(define-temporary-mode frame-transparency-mode
    ""
  :lighter (:eval
            (list mode-line-front-space
                  (propertize (format "α:%0.2f" (or (frame-parameter (selected-frame) 'alpha) 1))
                              'face 'lazy-highlight)))
  :install  "C-M-_"
  :bindings (("-" . make-frame-more-transparent)
             ("_" . make-frame-more-transparent)
             ("+" . make-frame-more-opaque)
             ("=" . make-frame-more-opaque)))

(define-temporary-mode frame-opaqueness-mode
    ""
  :lighter (:eval
            (list mode-line-front-space
                  (propertize (format "α:%0.2f" (or (frame-parameter (selected-frame) 'alpha) 1))
                              'face 'lazy-highlight)))
  :install  "C-M-+"
  :bindings (("-" . make-frame-more-transparent)
             ("_" . make-frame-more-transparent)
             ("+" . make-frame-more-opaque)
             ("=" . make-frame-more-opaque)))

(defun make-frame-more-transparent ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha
                       (let ((alpha (or (frame-parameter (selected-frame) 'alpha) 1))
                             (step 0.05)
                             (min 0.1))
                         (if (> alpha min)
                             (- alpha step)
                           alpha)))
  (redraw-modeline))

(defun make-frame-more-opaque ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha
                       (let ((alpha (or (frame-parameter (selected-frame) 'alpha) 1))
                             (step 0.05)
                             (max 1))
                         (if (< alpha max)
                             (+ alpha step)
                           alpha)))
  (redraw-modeline))
