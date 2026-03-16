;;; test-my-llm.el --- Tests for llm permission granting -*- lexical-binding: t; -*-

(require 'ert)
(require 'my-llm)

;; ---------------------------------------------------------------------------
;; Helper
;; ---------------------------------------------------------------------------

(defvar test-llm--root "/tmp/test-llm-project/")

(defmacro with-llm-project (&rest body)
  "Run BODY with a fresh llm project state and output buffer.
Sets up a fake project root, clean state, and tears down afterward."
  (declare (indent 0))
  `(let* ((llm--projects (make-hash-table :test 'equal))
          (root test-llm--root)
          (buf-name (llm--output-buffer-name root)))
     (unwind-protect
         (progn
           ;; Initialize project state
           (llm--project-state root)
           ;; Create output buffer with the mode
           (with-current-buffer (get-buffer-create buf-name)
             (llm-output-mode)
             (setq-local llm--buffer-project-root root)
             (let ((inhibit-read-only t))
               (erase-buffer)))
           ,@body)
       ;; Teardown
       (when (get-buffer buf-name)
         (kill-buffer buf-name))
       (remhash root llm--projects))))

;; ---------------------------------------------------------------------------
;; llm--build-args includes --permission-mode default
;; ---------------------------------------------------------------------------

(ert-deftest test-build-args-includes-permission-mode ()
  "llm--build-args should include --permission-mode acceptEdits."
  (let ((args (llm--build-args "hello")))
    (should (member "--permission-mode" args))
    (let ((pos (cl-position "--permission-mode" args :test #'equal)))
      (should (equal "acceptEdits" (nth (1+ pos) args))))))

;; ---------------------------------------------------------------------------
;; Face definition
;; ---------------------------------------------------------------------------

(ert-deftest test-llm-permission-face-exists ()
  "llm-indicator-permission-face should be defined."
  (should (facep 'llm-indicator-permission-face)))

;; ---------------------------------------------------------------------------
;; llm-permission-mode
;; ---------------------------------------------------------------------------

(ert-deftest test-llm-permission-mode-is-minor-mode ()
  "llm-permission-mode should be a minor mode."
  (with-temp-buffer
    (llm-permission-mode 1)
    (should llm-permission-mode)
    (llm-permission-mode -1)
    (should-not llm-permission-mode)))

(ert-deftest test-llm-permission-mode-binds-y-n ()
  "llm-permission-mode-map should bind y and n."
  (should (eq (lookup-key llm-permission-mode-map "y") #'llm-permission-allow))
  (should (eq (lookup-key llm-permission-mode-map "n") #'llm-permission-deny)))

;; ---------------------------------------------------------------------------
;; llm-output-mode-map bindings
;; ---------------------------------------------------------------------------

(ert-deftest test-llm-output-mode-map-has-allow-deny ()
  "llm-output-mode-map should have a/d bindings for permission."
  (should (eq (lookup-key llm-output-mode-map "a") #'llm-permission-allow))
  (should (eq (lookup-key llm-output-mode-map "d") #'llm-permission-deny)))

;; ---------------------------------------------------------------------------
;; llm--handle-non-json-line: basic insertion
;; ---------------------------------------------------------------------------

(ert-deftest test-handle-non-json-line-inserts-text ()
  "Non-JSON lines should be inserted into the output buffer."
  (with-llm-project
    (llm--handle-non-json-line "some plain text" root)
    (with-current-buffer (llm--output-buffer root)
      (should (string-match-p "some plain text" (buffer-string))))))

(ert-deftest test-handle-non-json-line-strips-box-drawing ()
  "Box-drawing characters should be stripped from non-JSON lines."
  (with-llm-project
    (llm--handle-non-json-line "╭─ Allow tool │ Bash ╰╯" root)
    (with-current-buffer (llm--output-buffer root)
      (let ((text (buffer-string)))
        (should (string-match-p "Allow tool" text))
        (should-not (string-match-p "[╭╮╰╯│─]" text))))))

(ert-deftest test-handle-non-json-line-skips-empty ()
  "Lines that become empty after stripping should not be inserted."
  (with-llm-project
    (llm--handle-non-json-line "╭──────╮" root)
    (with-current-buffer (llm--output-buffer root)
      (should (string-empty-p (buffer-string))))))

;; ---------------------------------------------------------------------------
;; llm--handle-non-json-line: permission detection
;; ---------------------------------------------------------------------------

(ert-deftest test-handle-non-json-line-detects-permission-yn ()
  "A line containing (y) should set :permission-pending."
  (with-llm-project
    (llm--handle-non-json-line "Allow once? (y) yes (n) no" root)
    (should (eq t (llm--project-get :permission-pending root)))))

(ert-deftest test-handle-non-json-line-detects-permission-allow ()
  "A line containing 'Allow' should set :permission-pending."
  (with-llm-project
    (llm--handle-non-json-line "Allow Bash tool?" root)
    (should (eq t (llm--project-get :permission-pending root)))))

(ert-deftest test-handle-non-json-line-activates-permission-mode ()
  "Permission detection should activate llm-permission-mode in the output buffer."
  (with-llm-project
    (llm--handle-non-json-line "Allow? (y)/(n)" root)
    (with-current-buffer (llm--output-buffer root)
      (should llm-permission-mode))))

(ert-deftest test-handle-non-json-line-no-permission-for-normal-text ()
  "Normal non-JSON text should not trigger permission state."
  (with-llm-project
    (llm--handle-non-json-line "Loading configuration..." root)
    (should-not (llm--project-get :permission-pending root))
    (with-current-buffer (llm--output-buffer root)
      (should-not llm-permission-mode))))

;; ---------------------------------------------------------------------------
;; llm--handle-non-json-line: indicator update
;; ---------------------------------------------------------------------------

(ert-deftest test-handle-non-json-line-updates-indicator ()
  "Permission detection should update the mode-line indicator to 'permission'."
  (with-llm-project
    ;; Create indicator using the current 2-argument signature.
    (with-current-buffer (llm--output-buffer root)
      (let ((inhibit-read-only t))
        (insert "test line"))
      (llm--region-indicator-create 'llm-indicator-pending-face root))
    (llm--handle-non-json-line "Allow? (y)/(n)" root)
    ;; The indicator is now a propertized string in llm--mode-line-indicators.
    (let ((str (cdr (assoc root llm--mode-line-indicators))))
      (should (string-match-p "permission" (or str ""))))
    ;; cleanup
    (llm--region-indicator-delete root)))

;; ---------------------------------------------------------------------------
;; JSON line routing: non-JSON goes to handler
;; ---------------------------------------------------------------------------

(ert-deftest test-handle-json-line-routes-non-json ()
  "llm--handle-json-line should route non-JSON to llm--handle-non-json-line."
  (with-llm-project
    (llm--handle-json-line "this is not json" root)
    (with-current-buffer (llm--output-buffer root)
      (should (string-match-p "this is not json" (buffer-string))))))

;; ---------------------------------------------------------------------------
;; JSON resume clears permission state
;; ---------------------------------------------------------------------------

(ert-deftest test-json-line-clears-permission-state ()
  "A valid JSON line should clear :permission-pending."
  (with-llm-project
    ;; Set up pending state
    (llm--project-set :permission-pending t root)
    (with-current-buffer (llm--output-buffer root)
      (llm-permission-mode 1))
    ;; Process a valid JSON line
    (llm--handle-json-line
     (json-encode '((type . "system") (session_id . "test-123")))
     root)
    (should-not (llm--project-get :permission-pending root))
    (with-current-buffer (llm--output-buffer root)
      (should-not llm-permission-mode))))

(ert-deftest test-json-line-no-clear-when-not-pending ()
  "Valid JSON should not error when no permission is pending."
  (with-llm-project
    ;; No permission pending — should just process normally
    (llm--handle-json-line
     (json-encode '((type . "system") (session_id . "test-456")))
     root)
    (should-not (llm--project-get :permission-pending root))))

;; ---------------------------------------------------------------------------
;; llm-permission-allow
;; ---------------------------------------------------------------------------

(ert-deftest test-permission-allow-sends-y ()
  "llm-permission-allow should send 'y\\n' to the process."
  (with-llm-project
    (let ((sent nil))
      ;; Mock a live process
      (let ((proc (start-process "test-cat" nil "cat")))
        (unwind-protect
            (progn
              (llm--project-set :process proc root)
              (llm--project-set :permission-pending t root)
              (with-current-buffer (llm--output-buffer root)
                (llm-permission-mode 1)
                (cl-letf (((symbol-function 'process-send-string)
                           (lambda (_proc str) (setq sent str))))
                  (llm-permission-allow)))
              (should (equal sent "y\n"))
              (should-not (llm--project-get :permission-pending root))
              (with-current-buffer (llm--output-buffer root)
                (should-not llm-permission-mode)
                (should (string-match-p "\\[allowed\\]" (buffer-string)))))
          (delete-process proc))))))

(ert-deftest test-permission-allow-errors-when-not-pending ()
  "llm-permission-allow should error when no permission is pending."
  (with-llm-project
    (with-current-buffer (llm--output-buffer root)
      (should-error (llm-permission-allow) :type 'user-error))))

;; ---------------------------------------------------------------------------
;; llm-permission-deny
;; ---------------------------------------------------------------------------

(ert-deftest test-permission-deny-sends-n ()
  "llm-permission-deny should send 'n\\n' to the process."
  (with-llm-project
    (let ((sent nil))
      (let ((proc (start-process "test-cat" nil "cat")))
        (unwind-protect
            (progn
              (llm--project-set :process proc root)
              (llm--project-set :permission-pending t root)
              (with-current-buffer (llm--output-buffer root)
                (llm-permission-mode 1)
                (cl-letf (((symbol-function 'process-send-string)
                           (lambda (_proc str) (setq sent str))))
                  (llm-permission-deny)))
              (should (equal sent "n\n"))
              (should-not (llm--project-get :permission-pending root))
              (with-current-buffer (llm--output-buffer root)
                (should-not llm-permission-mode)
                (should (string-match-p "\\[denied\\]" (buffer-string)))))
          (delete-process proc))))))

(ert-deftest test-permission-deny-errors-when-not-pending ()
  "llm-permission-deny should error when no permission is pending."
  (with-llm-project
    (with-current-buffer (llm--output-buffer root)
      (should-error (llm-permission-deny) :type 'user-error))))

;; ---------------------------------------------------------------------------
;; Full flow: non-JSON → permission pending → allow → JSON resumes
;; ---------------------------------------------------------------------------

(ert-deftest test-full-permission-flow-allow ()
  "Full flow: non-JSON triggers permission, allow clears it, JSON resumes."
  (with-llm-project
    (let ((sent nil))
      (let ((proc (start-process "test-cat" nil "cat")))
        (unwind-protect
            (progn
              (llm--project-set :process proc root)

              ;; Step 1: Non-JSON permission prompt arrives
              (llm--handle-json-line "╭─ Allow Bash tool? (y)/(n) ╰" root)
              (should (eq t (llm--project-get :permission-pending root)))
              (with-current-buffer (llm--output-buffer root)
                (should llm-permission-mode))

              ;; Step 2: User allows
              (with-current-buffer (llm--output-buffer root)
                (cl-letf (((symbol-function 'process-send-string)
                           (lambda (_proc str) (setq sent str))))
                  (llm-permission-allow)))
              (should (equal sent "y\n"))
              (should-not (llm--project-get :permission-pending root))
              (with-current-buffer (llm--output-buffer root)
                (should-not llm-permission-mode)
                (should (string-match-p "\\[allowed\\]" (buffer-string))))

              ;; Step 3: JSON output resumes normally
              (llm--handle-json-line
               (json-encode '((type . "assistant")
                              (message . ((content . (((type . "text")
                                                       (text . "Done."))))))))
               root)
              (with-current-buffer (llm--output-buffer root)
                (should (string-match-p "Done\\." (buffer-string)))))
          (delete-process proc))))))

(ert-deftest test-full-permission-flow-deny ()
  "Full flow: non-JSON triggers permission, deny clears it, JSON resumes."
  (with-llm-project
    (let ((sent nil))
      (let ((proc (start-process "test-cat" nil "cat")))
        (unwind-protect
            (progn
              (llm--project-set :process proc root)

              ;; Step 1: Permission prompt
              (llm--handle-json-line "│ Allow? (y) yes (n) no │" root)
              (should (eq t (llm--project-get :permission-pending root)))

              ;; Step 2: User denies
              (with-current-buffer (llm--output-buffer root)
                (cl-letf (((symbol-function 'process-send-string)
                           (lambda (_proc str) (setq sent str))))
                  (llm-permission-deny)))
              (should (equal sent "n\n"))
              (should-not (llm--project-get :permission-pending root))
              (with-current-buffer (llm--output-buffer root)
                (should (string-match-p "\\[denied\\]" (buffer-string))))

              ;; Step 3: JSON resumes
              (llm--handle-json-line
               (json-encode '((type . "assistant")
                              (message . ((content . (((type . "text")
                                                       (text . "OK, skipped."))))))))
               root)
              (with-current-buffer (llm--output-buffer root)
                (should (string-match-p "OK, skipped\\." (buffer-string)))))
          (delete-process proc))))))

;; ---------------------------------------------------------------------------
;; Process sentinel cleans up permission state
;; ---------------------------------------------------------------------------

(ert-deftest test-sentinel-clears-permission-on-exit ()
  "Process sentinel should clear permission state when process exits."
  (with-llm-project
    (let ((proc (start-process "test-true" nil "true")))
      (llm--project-set :process proc root)
      (llm--project-set :permission-pending t root)
      (process-put proc :llm-project-root root)
      (with-current-buffer (llm--output-buffer root)
        (llm-permission-mode 1))
      ;; Wait for process to finish
      (while (process-live-p proc)
        (accept-process-output proc 0.1))
      ;; Manually call sentinel (it fires async, force it for test)
      (llm--process-sentinel proc "finished\n")
      (should-not (llm--project-get :permission-pending root))
      (with-current-buffer (llm--output-buffer root)
        (should-not llm-permission-mode)))))

;; ---------------------------------------------------------------------------
;; Multiple permission cycles
;; ---------------------------------------------------------------------------

(ert-deftest test-multiple-permission-cycles ()
  "Should handle multiple permission prompts in sequence."
  (with-llm-project
    (let ((sent-list nil))
      (let ((proc (start-process "test-cat" nil "cat")))
        (unwind-protect
            (progn
              (llm--project-set :process proc root)

              ;; First permission prompt
              (llm--handle-non-json-line "Allow Bash? (y)/(n)" root)
              (should (llm--project-get :permission-pending root))
              (with-current-buffer (llm--output-buffer root)
                (cl-letf (((symbol-function 'process-send-string)
                           (lambda (_proc str) (push str sent-list))))
                  (llm-permission-allow)))
              (should-not (llm--project-get :permission-pending root))

              ;; JSON flows
              (llm--handle-json-line
               (json-encode '((type . "system") (session_id . "s1")))
               root)

              ;; Second permission prompt
              (llm--handle-non-json-line "Allow Read? (y)/(n)" root)
              (should (llm--project-get :permission-pending root))
              (with-current-buffer (llm--output-buffer root)
                (cl-letf (((symbol-function 'process-send-string)
                           (lambda (_proc str) (push str sent-list))))
                  (llm-permission-deny)))
              (should-not (llm--project-get :permission-pending root))

              ;; Verify both sends happened
              (should (equal (nreverse sent-list) '("y\n" "n\n"))))
          (delete-process proc))))))

;; ---------------------------------------------------------------------------
;; llm-new-session
;; ---------------------------------------------------------------------------

(ert-deftest test-llm-new-session-clears-session-id ()
  "llm-new-session should set :session-id to nil."
  (with-llm-project
    (llm--project-set :session-id "old-session-123" root)
    (with-current-buffer (llm--output-buffer root)
      (llm-new-session))
    (should (null (llm--project-get :session-id root)))))

;; ---------------------------------------------------------------------------
;; llm-copy-response
;; ---------------------------------------------------------------------------

(ert-deftest test-llm-copy-response-copies-to-kill-ring ()
  "llm-copy-response should place the last response on the kill ring."
  (with-llm-project
    (with-current-buffer (llm--output-buffer root)
      (let ((inhibit-read-only t))
        (insert (format "* %s a question\n\nThis is the answer.\n"
                        llm--prompt-keyword))))
    (with-current-buffer (llm--output-buffer root)
      (llm-copy-response))
    (should (equal (car kill-ring) "This is the answer."))))

;; ---------------------------------------------------------------------------
;; llm--last-response: PROPERTIES block skip
;; ---------------------------------------------------------------------------

(ert-deftest test-last-response-skips-properties-block ()
  "llm--last-response should not include :PROPERTIES:...:END: in the result."
  (with-llm-project
    (with-current-buffer (llm--output-buffer root)
      (let ((inhibit-read-only t))
        (insert (format "* %s a question\n:PROPERTIES:\n:session:   abc123\n:cost:      $0.01\n:END:\n\nThe real response.\n"
                        llm--prompt-keyword))))
    (let ((response (llm--last-response root)))
      (should (string-match-p "The real response\\." response))
      (should-not (string-match-p ":PROPERTIES:" response))
      (should-not (string-match-p ":END:" response)))))

(provide 'test-my-llm)
;;; test-my-llm.el ends here
