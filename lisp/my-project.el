;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'f)
(require 's)
(require 'files)
(require 'load-relative)

(defclass my-project ()
  ((name :initarg :name :type symbol :reader my-project-name)
   (root :initarg :root :type string :reader my-project-root)))

(cl-defmacro defproject (project &key dependencies &allow-other-keys)
  (declare (indent 1) (debug t))
  (cl-loop
     with header = `(progn
                      (defvar ,project (my-project :name (quote ,project)
                                                   :root (file-name-directory (__FILE__)))))
     for dependency across dependencies
     collect `(funcall (-orfn  ;; Tryouts

                        (lambda (dependency)
                          (let ((feature (intern (format "%s:%s" (quote ,project) (quote ,dependency)))))
                            (message "Search feature in a project load path: %s" feature)
                            (condition-case nil
                                (progn (require feature)
                                       t)
                              (file-missing (message "Feature not found in a load path: %s" feature) nil)
                              (file-error (message "Feature error when loading feature: %s" feature) nil))))

                        (lambda (dependency)
                          (message "Search feature in a global load path: %s" dependency)
                          (condition-case nil
                              (progn (require dependency)
                                     t)
                            (file-missing (message "Dependency not found in a load path: %s" dependency)
                                          nil)))

                        (lambda (dependency)
                          (let* ((dependency-path (concat (s-replace "." "/" (symbol-name dependency)) ".el"))
                                 (filename (f-join (my-project-root ,project) dependency-path))
                                 (feature (intern (format "%s:%s" (quote ,project) (quote ,dependency)))))
                            (message "Load feature %s from file: %s" feature filename)
                            (condition-case nil
                                (progn (let ((load-path (append load-path (list (f-dirname filename)))))
                                         (load-file filename)
                                         (provide feature))
                                       ;; (cl-pushnew (f-dirname filename) load-path)
                                       t)
                              (file-missing (message "Dependency not found: %s" filename)
                                            nil)
                              (file-error (message "File error: %s" filename)
                                          nil))))

                        (lambda (dependency)
                          (let* ((filename (f-join (my-project-root ,project)
                                                   (concat (s-replace "." "/" (symbol-name dependency)))
                                                   (concat (car (last (s-split "\\." (symbol-name dependency))))
                                                           ".el")))
                                 (feature (intern (format "%s:%s" (quote ,project) (quote ,dependency)))))
                            (condition-case nil
                                (progn (let ((load-path (append load-path (list (f-dirname filename)))))
                                         (load-file filename)
                                         (provide feature))
                                       ;; (cl-pushnew (f-dirname filename) load-path)
                                       t)
                              (file-missing (message "Dependency not found: %s" filename)
                                            nil)
                              (file-error (message "File error: %s" filename)
                                          nil))))

                        (lambda (dependency)
                          (message "Installing package: %s" dependency)
                          (condition-case nil
                              (progn (use-package ,dependency)
                                     t)
                            (error (message "Package %s is unavailable for use package" dependency)
                                   nil)))

                        (lambda (dependency)
                          (warn "Package %s is unavailable" dependency)))
                       (quote ,dependency))
     into forms
     finally (return (append header forms))))

(provide 'my-project)
