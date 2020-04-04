;;; org-glance.el --- org-mode traversing. Fast and convenient.

;; Copyright (C) 2018-2020 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 1.0

;; Keywords: org-mode tools
;; Homepage: https://github.com/rails-to-cosmos/org-glance

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(require 'org)
(require 'seq)
(require 'cl-lib)
(require 'load-relative)

(eval-when-compile
  (require 'aes)
  (require 'cl-generic)
  (require 'dash-functional)
  (require 'subr-x))

(load-relative "plugins/org-glance-password-manager.el")

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance--views '())

(defconst org-glance-property--glance-dir
  "GLANCE_DIR")

(defvar org-glance--default-scope-alist
  `((file-with-archives . org-glance-scope--list-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . org-glance-scope--agenda-with-archives)))

(define-error 'org-glance-cache-outdated
  "Cache file is outdated"
  'user-error)

(defun org-glance-cache-outdated (format &rest args)
  (signal 'org-glance-cache-outdated
          (list (apply #'format-message format args))))

(defun org-glance--get-directories-from-headers (filename)
  (let* ((default-directory (file-name-directory filename))
         (glance-dir-property (format "#+%s:" org-glance-property--glance-dir)))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (loop while (search-forward glance-dir-property nil t)
            with glance-archive-dir
            do (setq glance-archive-dir
                     (-some->> (thing-at-point 'line)
                       substring-no-properties
                       (s-replace glance-dir-property "")
                       s-trim
                       file-truename))
            if (file-exists-p glance-archive-dir)
            collect glance-archive-dir into result
            else
            do (warn "glance-archive-dir from %s not found: %s" filename glance-archive-dir)
            finally (return (or result (list default-directory)))))))

(defun org-glance-scope--list-file-archives (filename)
  (let* ((archive-dirs (org-glance--get-directories-from-headers filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension))
         (archive-filename (-some->> base-filename
                             (s-append ".org_archive")))
         (org-filename (-some->> base-filename
                         (s-append ".org"))))
    (loop for archive-dir in archive-dirs
          append (directory-files-recursively archive-dir archive-filename)
          append (directory-files-recursively archive-dir org-filename))))

(defun org-glance-scope--list-archives ()
  (append (list (buffer-file-name))
          (org-glance-scope--list-file-archives (buffer-file-name))))

(defun org-glance-scope--agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
           append (list filename)
           append (org-glance-scope--list-file-archives filename)))

(cl-defgeneric org-glance-adapt-scope (lfob)
  "Adapt list-file-or-buffer to list of file-or-buffers.")

(cl-defmethod org-glance-adapt-scope ((lfob string))
  "Return list of file LFOB if exists."
  (list (or (expand-file-name lfob)
            (-some->> lfob
              expand-file-name
              get-file-buffer
              buffer-name))))

(cl-defmethod org-glance-adapt-scope ((lfob sequence))
  "Adapt each element of LFOB."
  (-some->> lfob
    (-keep #'(lambda (fob) (->> fob org-glance-adapt-scope)))
    (-flatten)
    (seq-uniq)))

(cl-defmethod org-glance-adapt-scope ((lfob symbol))
  "Return extracted LFOB from `org-glance--default-scope-alist'."
  (-some->> lfob
    (funcall (-cut alist-get <> org-glance--default-scope-alist))
    (funcall)))

(cl-defmethod org-glance-adapt-scope ((lfob buffer))
  "Return list of LFOB."
  (list
   (condition-case nil
       (get-file-buffer lfob)
     (error lfob))))

(cl-defmethod org-glance-adapt-scope ((lfob function))
  "Adapt result of LFOB."
  (-some->> lfob
    funcall
    org-glance-adapt-scope))

(cl-defun org-glance-serialize (headline &key title-property)
  (prin1-to-string
   (list (when title-property
           (org-element-property title-property headline))
         (org-element-property :raw-value headline)
         (org-element-property :begin headline)
         (org-element-property :file headline))))

(cl-defun org-glance-deserialize (input &key title-property)
  (cl-destructuring-bind (alias title begin file) input
    (org-element-create 'headline
                        `(,title-property ,alias
                                          :raw-value ,title
                                          :begin ,begin
                                          :file ,file))))

(cl-defun org-glance-completing-read (headlines &key prompt title-property)
  (org-completing-read prompt
                       (cl-loop for headline in headlines
                                collect (org-glance-format headline :title-property title-property))))

(cl-defun org-glance-format (headline &key title-property)
  (or (and title-property (org-element-property title-property headline))
      (org-element-property :raw-value headline)))

(cl-defun org-glance-browse (headlines &key choice fallback title-property)
  (or (cl-loop for headline in headlines
               when (string= (org-glance-format headline :title-property title-property) choice)
               do (cl-return headline))
      (when fallback (funcall fallback choice))))

(cl-defgeneric org-glance-read (file &key filter)
  "Read org-element headlines from one or many files.")

(cl-defmethod org-glance-read ((files list) &key filter)
  (cl-loop for file in (org-glance-adapt-scope files)
           do (message "Glance %s" file)
           append (org-glance-read file :filter filter) into result
           do (redisplay)
           finally (cl-return result)))

(cl-defmethod org-glance-read ((file string) &key filter)
  (pcase-let ((`(,file ,id) (s-split-up-to "#" file 2)))
    (when (and (file-exists-p file)
               (not (f-directory? file)))
      (with-temp-buffer
        (insert-file-contents file)
        (when id
          (goto-char (org-find-entry-with-id id))
          (org-narrow-to-subtree))
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (headline)
            (when-let (headline (if filter
                                    (when (funcall filter headline)
                                      headline)
                                  headline))
              (plist-put (cadr headline) :file file)
              headline)))))))

(cl-defun org-glance-save (file entries &key title-property)
  (unless (file-exists-p (file-name-directory file))
    (make-directory (file-name-directory file) t))
  (with-temp-file file
    (insert "`(")
    (dolist (entry entries)
      (insert (org-glance-serialize entry
                                    :title-property title-property) "\n"))
    (insert ")"))
  entries)

(cl-defun org-glance-load (file &key title-property)
  (let ((entries
         (with-temp-buffer (insert-file-contents file)
                           (->> (buffer-string)
                                substring-no-properties
                                read
                                eval))))
    (cl-loop for entry in entries
             collect (org-glance-deserialize entry
                                             :title-property title-property))))

(defun org-glance--element-at-point-equals-headline (headline)
  (condition-case nil
      (s-contains? (org-element-property :raw-value (org-element-at-point))
                   (org-element-property :raw-value headline))
    (error nil)))

(defun org-glance-act--visit-headline (headline)
  "Goto HEADLINE."
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (file-buffer (get-file-buffer file)))

    (cond ((file-exists-p file)
           (find-file file))
          (t
           (org-glance-cache-outdated "File not found: %s" file)))

    (widen)
    (goto-char point)

    (cond ((org-glance--element-at-point-equals-headline headline)
           (org-narrow-to-subtree)
           (org-show-all)
           (widen))
          (t
           (unless file-buffer
             (kill-buffer))
           (org-glance-cache-outdated "Cache file is outdated")))))

(defun org-glance-act--open-org-link (headline)
  "Open org-link at HEADLINE."
  (let* ((file (org-element-property :file headline))
         (file-buffer (get-file-buffer file))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
    (org-glance-act--visit-headline headline)
    (org-open-at-point)
    (if file-buffer
        (bury-buffer file-buffer)
      (kill-buffer (get-file-buffer file)))))

(defun org-glance-sync-materialized-subtree ()
  (interactive)
  (save-excursion

    (unless (org-at-heading-p)
      (org-back-to-heading))

    (let* ((source (org-entry-get (point) "ORG_GLANCE_SOURCE"))
           (beg (condition-case nil
                    (string-to-number (org-entry-get (point) "ORG_GLANCE_BEG"))
                  (error (user-error "Materialized properties corrupted, please reread"))))
           (end (condition-case nil
                    (string-to-number (org-entry-get (point) "ORG_GLANCE_END"))
                  (error (user-error "Materialized properties corrupted, please reread"))))
           (glance-hash (org-entry-get (point) "ORG_GLANCE_HASH"))
           (mat-hash (save-restriction
                       (org-narrow-to-subtree)
                       (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                         (with-temp-buffer
                           (org-mode)
                           (insert buffer-contents)
                           (goto-char (point-min))

                           (org-delete-property "ORG_GLANCE_SOURCE")
                           (org-delete-property "ORG_GLANCE_BEG")
                           (org-delete-property "ORG_GLANCE_END")
                           (org-delete-property "ORG_GLANCE_HASH")

                           ;; (prin1 (buffer-substring-no-properties (point-min) (point-max)))
                           (buffer-hash)))))
           (src-hash (with-temp-buffer
                       (org-mode)
                       (insert-file-contents source)
                       (let ((subtree (condition-case nil
                                          (buffer-substring-no-properties beg end)
                                        (error (user-error "Materialized properties corrupted, please reread")))))
                         (with-temp-buffer
                           (org-mode)
                           (insert subtree)
                           (goto-char (point-min))
                           (while
                               (condition-case nil
                                   (org-with-limited-levels
                                    (org-map-tree 'org-promote)
                                    t)
                                 (error nil))
                             t)
                           ;; (prin1 (buffer-substring-no-properties (point-min) (point-max)))
                           (buffer-hash))))))

      (when (not (string= glance-hash src-hash))
        (user-error "Source file modified or materialized properties corrupted, please reread"))

      (when (string= glance-hash mat-hash)
        (user-error "No changes made in subtree"))

      (message "Subtree has been changed"))))

(defun org-glance-scope-materialize (filename)
  (let ((headlines (org-glance-load filename))
        (file-entries (make-hash-table))
        (output-filename (make-temp-file "org-glance-materialized-" nil ".org")))

    (loop for hl in headlines
          do (let ((fn (intern (org-element-property :file hl)))
                   (pos (org-element-property :begin hl)))
               (puthash fn (cons pos (gethash fn file-entries)) file-entries)))

    (maphash (lambda (file entries)
               (with-temp-buffer
                 (org-mode)
                 (insert-file-contents (symbol-name file))
                 (loop for pos in entries
                       do (let* ((beg (save-excursion
                                        (goto-char pos)
                                        (beginning-of-line)
                                        (point)))
                                 (end (save-excursion
                                        (goto-char pos)
                                        (org-end-of-subtree)
                                        (point)))
                                 (contents (buffer-substring-no-properties beg end)))
                            (with-temp-buffer
                              (org-mode)
                              (insert contents)
                              (goto-char (point-min))
                              (while
                                  (condition-case nil
                                      (org-with-limited-levels
                                       (org-map-tree 'org-promote)
                                       t)
                                    (error nil))
                                t)

                              (let ((hash (buffer-hash)))
                                (goto-char (point-min))
                                (org-set-property "ORG_GLANCE_SOURCE" (symbol-name file))
                                (org-set-property "ORG_GLANCE_BEG" (number-to-string beg))
                                (org-set-property "ORG_GLANCE_END" (number-to-string end))
                                (org-set-property "ORG_GLANCE_HASH" hash)
                                (goto-char (point-max))
                                (insert "\n")
                                (append-to-file (point-min) (point-max) output-filename)))))))
             file-entries)

    (with-current-buffer (find-file-other-window output-filename)
      (org-mode)
      (set-mark (point-min))
      (goto-char (point-max))
      (org-sort-entries nil ?a)
      (deactivate-mark)
      (org-overview))))

(defun org-glance-sec--decrypt-current-headline (&optional return-plain)
  "Decrypt encrypted `org-mode` subtree at point.
If RETURN-PLAIN is non-nil, return decrypted contents as string."
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (encrypted (let ((encrypted (buffer-substring-no-properties beg end)))
                      (if (not (with-temp-buffer
                                 (insert encrypted)
                                 (aes-is-encrypted)))
                          (user-error "Headline is not encrypted")
                        encrypted)))
         (plain (aes-decrypt-buffer-or-string encrypted)))
    (unless plain
      (user-error "Wrong password"))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert plain)
      (if return-plain
          plain
        t))))

(defun org-glance-sec--encrypt-current-headline ()
  "Encrypt org subtree contents at point."
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (plain (let ((plain (buffer-substring-no-properties beg end)))
                  (if (with-temp-buffer
                        (insert plain)
                        (aes-is-encrypted))
                      (user-error "Headline is already encrypted")
                    plain)))
         (encrypted (aes-encrypt-buffer-or-string plain)))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert encrypted))))

(defun org-glance-sec--extract (headline)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (org-element-property :file headline))
    (goto-char (org-element-property :begin headline))
    (org-narrow-to-subtree)
    (let ((tf (make-temp-file "org-glance-pm"))
          (dc (org-glance-sec--decrypt-current-headline t)))
      (unwind-protect
          (with-temp-file tf
            (insert dc))
        (while
            (condition-case exc
                (org-glance :scope tf
                            :prompt "Copy to kill ring: "
                            :action #'org-glance-pm--copy)
              (quit (kill-new "" t) nil)
              (error (kill-new "" t) nil)))
        (delete-file tf)))))

(defun org-glance-sec--copy (headline)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (org-element-property :file headline))
    (goto-char (org-element-property :begin headline))
    (let* ((beg (save-excursion (org-end-of-line) (1+ (point))))
           (end (save-excursion (org-end-of-subtree t)))
           (contents (buffer-substring-no-properties beg end)))
      (kill-new contents t))))

(cl-defun org-glance-cache-reread (&key scope filter cache-file title-property &allow-other-keys)
  (let ((headlines (org-glance-read scope :filter filter)))

    (unless headlines
      (user-error "Nothing to glance at scope %s" (pp-to-string scope)))

    (when cache-file
      (org-glance-save cache-file headlines :title-property title-property))

    headlines))

(cl-defun org-glance (&key
                      filter
                      fallback
                      default-choice
                      cache-file
                      force-reread-p
                      (scope '(agenda))
                      (action #'org-glance-act--visit-headline)
                      (prompt "Glance: ")
                      (title-property :TITLE))
  "Run completing read on org-files entries from SCOPE list prompting a PROMPT.
Scope can be file name or list of file names.
Filter headlines by FILTER method.
Call ACTION method on selected headline.
Specify CACHE-FILE to save headlines to read-optimized el-file.
Specify FORCE-REREAD-P predicate to reread cache file. Usually this flag is set by C-u prefix.
If user input doesn't match any entry, call FALLBACK method with user input as argument.
Read headline title in completing read prompt from org-property TITLE-PROPERTY."

  (let (headlines)
    (when (or force-reread-p (not cache-file) (not (file-exists-p cache-file)))
      (when (and force-reread-p cache-file)
        (message "Reread cache file %s..." cache-file))
      (setq headlines
            (org-glance-cache-reread
             :scope scope
             :filter filter
             :cache-file cache-file
             :title-property title-property)))

    (unless headlines
      (setq headlines (org-glance-load cache-file :title-property title-property)))

    (unless headlines
      (user-error "Nothing to glance at (scope: %s)" scope))

    (unwind-protect
        (when-let (choice (or default-choice
                              (org-glance-completing-read headlines
                                                          :prompt prompt
                                                          :title-property title-property)))
          (if-let (headline (org-glance-browse headlines
                                               :choice choice
                                               :fallback fallback
                                               :title-property title-property))
              (condition-case nil
                  (if (functionp action)
                      (funcall action headline)
                    (user-error "Specify ACTION method to call on headline"))
                (org-glance-cache-outdated
                 (message "Cache file %s is outdated, actualizing..." cache-file)
                 (redisplay)
                 (org-glance
                  :scope scope
                  :prompt prompt
                  :filter filter
                  :action action
                  :cache-file cache-file
                  :fallback fallback
                  :default-choice choice
                  :title-property title-property
                  :force-reread-p t)))
            (user-error "Headline not found")))
      ;; Unwind
      (when (and cache-file
                 (or force-reread-p
                     (not (file-exists-p cache-file))))
        (org-glance-save cache-file headlines :title-property title-property)))))

(defun org-glance-list-views ()
  (interactive)
  (let ((view (org-completing-read "View: " org-glance--views)))
    (funcall (intern (format "org-glance--%s-visit" (s-downcase view))))))

(cl-defmacro org-glance-def-view (tag &key bind encrypted type &allow-other-keys)
  (declare (indent 1))
  (let* ((dtag (s-downcase tag))
         (ctag (s-capitalize tag))
         (ns (format "org-glance--%s-" dtag))

         ;; default params
         (cache-file-name (format "~/.emacs.d/org-glance/org-glance-%s.el" dtag))
         (scope '(agenda-with-archives))

         ;; function names
         (fn-open (intern (concat ns "open")))
         (fn-reread (intern (concat ns "reread")))
         (fn-fallback (intern (concat ns "fallback")))
         (fn-filter (intern (concat ns "filter")))
         (fn-visit (intern (concat ns "visit")))
         (fn-materialize (intern (concat ns "materialize")))

         ;; only for encrypted views
         (fn-encrypt-current-headline (intern (concat ns "encrypt-current-headline")))
         (fn-decrypt-current-headline (intern (concat ns "decrypt-current-headline")))
         (fn-decrypt-extract (intern (concat ns "decrypt-extract"))))

    (add-to-list 'org-glance--views (intern tag))

    `(progn

       (defun ,fn-filter (headline)
         (-contains? (mapcar #'s-downcase (org-element-property :tags headline)) ,dtag))

       (defun ,fn-fallback (_)
         (user-error "%s not found." ,ctag))

       (defun ,fn-visit (&optional force-reread-p)
         (interactive "P")
         (org-glance
          :scope (quote ,scope)
          :prompt ,(format "Visit %s: " dtag)
          :cache-file ,cache-file-name
          :force-reread-p force-reread-p
          :filter (function ,fn-filter)
          :fallback (function ,fn-fallback)
          :action (function org-glance-act--visit-headline)))

       (defun ,fn-reread ()
         (interactive)
         (org-glance-cache-reread
          :scope (quote ,scope)
          :filter (function ,fn-filter)
          :cache-file ,cache-file-name))

       (defun ,fn-materialize ()
         (interactive)
         (,fn-reread)
         (org-glance-scope-materialize ,cache-file-name))

       (when ,(cond ((symbolp type) (eq type 'link))
                    ((listp type) (-contains? type 'link))
                    (t nil))

         (defun ,fn-open (&optional force-reread-p)
           (interactive "P")
           (org-glance
            :scope (quote ,scope)
            :prompt ,(format "Open %s: " dtag)
            :cache-file ,cache-file-name
            :force-reread-p force-reread-p
            :filter (function ,fn-filter)
            :fallback (function ,fn-fallback)
            :action (function org-glance-act--open-org-link))))

       (when ,encrypted ;; methods for encrypted views

         (defun ,fn-encrypt-current-headline ()
           (interactive)
           (org-glance-sec--encrypt-current-headline))

         (defun ,fn-decrypt-current-headline ()
           (interactive)
           (org-glance-sec--decrypt-current-headline))

         (when ,(cond ((symbolp type) (eq type 'kv))
                      ((listp type) (-contains? type 'kv))
                      (t nil))
           (defun ,fn-decrypt-extract (&optional force-reread-p)
             (interactive "P")
             (org-glance
              :scope (quote ,scope)
              :prompt ,(format "Decrypt and extract %s: " dtag)
              :cache-file ,cache-file-name
              :force-reread-p force-reread-p
              :filter (function ,fn-filter)
              :fallback (function ,fn-fallback)
              :action #'org-glance-sec--extract))))

       (when (quote ,bind)
         (cl-loop for (k . cmd) in (quote ,bind)
                  do (global-set-key (kbd k) cmd))))))

(provide-me)
;;; org-glance.el ends here
