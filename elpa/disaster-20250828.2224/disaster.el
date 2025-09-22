;;; disaster.el --- Disassemble C, C++ or Fortran code under cursor -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2023 Justine Tunney.

;; Author: Justine Tunney <jtunney@gmail.com>
;;         Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Maintainer: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Created: 2013-03-02
;; Package-Version: 20250828.2224
;; Package-Revision: 0299c129d415
;; Package-Requires: ((emacs "27"))
;; Keywords: tools c
;; URL: https://github.com/jart/disaster

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ![Screenshot of a C example](screenshot-c.png)
;;
;; ![Screenshot of a Fortran example](screenshot-fortran.png)
;;
;; Disaster lets you press `C-c d` to see the compiled assembly code for the
;; C, C++ or Fortran file you're currently editing. It even jumps to and
;; highlights the line of assembly corresponding to the line beneath your cursor.
;;
;; It works in the following manner:

;; - If there is a `Makefile`, creating a `.o` file using `make`
;; - If there is a `compile_commands.json` file, use it to get the compilation
;;   command for the file.
;; - If none of the above files is presnet, use the default system compiler.

;; After compiling the source to a `.o` file, `disaster' runs that file through
;; `objdump` to generate the human-readable assembly.

;;; Installation:

;; Make sure to place `disaster.el` somewhere in the `load-path`, then you should
;; be able to run `M-x disaster`. If you want, you add the following lines to
;; your `.emacs` file to register the `C-c d` shortcut for invoking `disaster`:
;;
;; ```elisp
;; (add-to-list 'load-path "/PATH/TO/DISASTER")
;; (require 'disaster)
;; (define-key c-mode-map (kbd "C-c d") 'disaster)
;; (define-key fortran-mode-map (kbd "C-c d") 'disaster)
;; ```

;; #### Doom Emacs

;; For Doom Emacs users, you can add this snippet to your `packages.el`.
;;
;; ```elisp
;; (package! disaster
;;   :recipe (:host github :repo "jart/disaster"))
;; ```
;;
;; And this to your `config.el`:
;;
;; ```elisp
;; (use-package! disaster
;;   :commands (disaster)
;;   :init
;;   ;; If you prefer viewing assembly code in `nasm-mode` instead of `asm-mode`
;;   (setq disaster-assembly-mode #'nasm-mode)
;;
;;   (map! :localleader
;;         :map (c++-mode-map c-mode-map fortran-mode-map)
;;         :desc "Disaster" "d" #'disaster))
;; ```

;;; Code:

(require 'json)
(require 'vc)

(define-obsolete-function-alias 'disaster-create-compile-command-cmake
  'disaster-create-compile-command-compile-db "1.2")

(define-obsolete-function-alias 'disaster-get-object-file-path-cmake
  'disaster-get-object-file-path-compile-db "1.2")

(defun disaster--is-m1? ()
  "Identify Mac M1 chips."
  (and (eq system-type 'darwin)
       (string= "aarch64" (car (split-string system-configuration "-")))))

(defun disaster--arch-flags ()
  "Select the right flags depending on the right architecture."
  (if (disaster--is-m1?)
      "-mcpu=apple-m1"
    "-march=native"))

(defgroup disaster nil
  "Disassemble C/C++ under cursor (Works best with Clang)."
  :prefix "disaster-"
  :group 'tools)

(defcustom disaster-make-flags "-k"
  "Command line options to pass to make if a Makefile is found."
  :group 'disaster
  :type 'string)

(defcustom disaster-meson-build-directory "build"
  "Name of the Meson build directory."
  :group 'disaster
  :type 'string)

(defcustom disaster-assembly-mode #'asm-mode
  "Which mode to use to view assembly code."
  :group 'disaster
  :type 'function)

(defcustom disaster-cc (or (getenv "CC") "cc")
  "The command for your C compiler."
  :group 'disaster
  :type 'string)

(defcustom disaster-cxx (or (getenv "CXX") "c++")
  "The command for your C++ compiler."
  :group 'disaster
  :type 'string)


(defcustom disaster-fortran (or (getenv "FORTRAN") "gfortran")
  "The command for your Fortran compiler."
  :group 'disaster
  :type 'string)

(defcustom disaster-cflags (or (getenv "CFLAGS")
                               (disaster--arch-flags))
  "Command line options to use when compiling C."
  :group 'disaster
  :type 'string)

(defcustom disaster-cxxflags (or (getenv "CXXFLAGS")
                                 (disaster--arch-flags))
  "Command line options to use when compiling C++."
  :group 'disaster
  :type 'string)


(defcustom disaster-fortranflags (or (getenv "FORTRANFLAGS")
                                     (disaster--arch-flags))
  "Command line options to use when compiling Fortran."
  :group 'disaster
  :type 'string)

(defcustom disaster-objdump
  (concat (if (and (eq system-type 'darwin)
                   (not (disaster--is-m1?)))
              "gobjdump"
            "objdump")
          " -d -M att -Sl --no-show-raw-insn")
  "The command name and flags for running objdump."
  :group 'disaster
  :type 'string)

(defcustom disaster-buffer-compiler "*disaster-compilation*"
  "Buffer name to use for assembler output."
  :group 'disaster
  :type 'string)

(defcustom disaster-buffer-assembly "*disaster-assembly*"
  "Buffer name to use for objdump assembly output."
  :group 'disaster
  :type 'string)

(defcustom disaster-project-root-files
  (list (list ".projectile")    ;; Projectile project root.
        (list "setup.py"        ;; Python apps.
              "package.json")   ;; node.js apps.
        (list "CMakeLists.txt") ;; CMake files are sometimes in subdirectories.
        (list "Makefile"))      ;; Makefiles are sometimes in subdirectories.
  "List of lists of files that may indicate software project root directory.
Sublist are ordered from highest to lowest precedence."
  :group 'disaster
  :type '(repeat (repeat string)))

(defcustom disaster-c-regexp "\\.c$"
  "Regexp for C source files."
  :group 'disaster
  :type 'regexp)

(defcustom disaster-cpp-regexp "\\.c\\(c\\|pp\\|xx\\)$"
  "Regexp for C++ source files."
  :group 'disaster
  :type 'regexp)

(defcustom disaster-fortran-regexp "\\.f\\(or\\|90\\|95\\|0[38]\\)?$"
  "Regexp for Fortran source files."
  :group 'disaster
  :type 'regexp)

;;;###autoload
(defvar disaster-find-build-root-functions nil
  "Functions to call to get the build root directory from the project directory.
If nil is returned, the next function will be tried.  If all
functions return nil, the project root directory will be used as
the build directory.")

(defun disaster-create-compile-command-make (make-root cwd rel-obj obj-file _proj-root _rel-file file)
  "Create compile command for a Make-based project.
MAKE-ROOT: path to build root,
CWD: path to current source file,
REL-OBJ: path to object file (relative to project root),
OBJ-FILE: full path to object file (build root!)
PROJ-ROOT: path to project root, REL-FILE FILE."
  (if make-root
      ;; if-then
      (cond ((equal cwd make-root)
             (format "make %s %s" disaster-make-flags (shell-quote-argument rel-obj)))
            (t (format "make %s -C %s %s"
                       disaster-make-flags make-root rel-obj)))
    ;; if-else
    (cond ((string-match-p disaster-cpp-regexp file)
           (format "%s %s -g -c -o %s %s"
                   disaster-cxx disaster-cxxflags
                   (shell-quote-argument obj-file) (shell-quote-argument file)))
          ((string-match-p disaster-c-regexp file)
           (format "%s %s -g -c -o %s %s"
                   disaster-cc disaster-cflags
                   (shell-quote-argument obj-file) (shell-quote-argument file)))
          ((string-match-p disaster-fortran-regexp file)
           (format "%s %s -g -c -o %s %s"
                   disaster-fortran disaster-fortranflags
                   (shell-quote-argument obj-file) (shell-quote-argument file)))
          (t (warn "File %s do not seems to be a C, C++ or Fortran file." file)))))

(defun disaster-create-compile-command-compile-db (make-root _cwd _rel-obj _obj-file proj-root rel-file)
  "Create compile command from a \"compile_commands.json\" file.
MAKE-ROOT: path to build root,
CWD: path to current source file,
REL-OBJ: path to object file (relative to project root),
OBJ-FILE: full path to object file (build root!)
PROJ-ROOT: path to project root, REL-FILE FILE."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file (expand-file-name "compile_commands.json" make-root))))
    (catch 'compile-command
      (dolist (obj json)
        (when (string-equal (file-relative-name (gethash "file" obj) proj-root) rel-file)
          (throw 'compile-command
                 (if-let ((argv (gethash "arguments" obj)))
                     (mapconcat (lambda (s) (concat "\"" s "\"")) argv " ")
                   (gethash "command" obj))))))))

(defun disaster-create-compile-command-meson (make-root _cwd _rel-obj _obj-file _proj-root rel-file)
  "Create compile command from Meson.
MAKE-ROOT: path to build root,
CWD: path to current source file,
REL-OBJ: path to object file (relative to build root),
OBJ-FILE: full path to object file (build root!)
PROJ-ROOT: path to project root, REL-FILE FILE."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file (expand-file-name "compile_commands.json" make-root))))
    (catch 'compile-command
      (dolist (obj json)
        (let ((this-file (gethash "file" obj)))
          (when (string-equal this-file rel-file)
            (throw 'compile-command
                   (if-let ((argv (gethash "arguments" obj)))
                       (mapconcat (lambda (s) (concat "\"" s "\"")) argv " ")
                     (gethash "command" obj)))))))))

(defun disaster-get-object-file-path-compile-db (compile-cmd)
  "Get the .o object file name from a full COMPILE-CMD."
  (let* ((parts (reverse (split-string-and-unquote compile-cmd " ")))
         (prev nil))
    (catch 'object-file
      (dolist (part parts)
        (when (member part '("-o" "--output")) (throw 'object-file prev))
        (setq prev part)))))

(defun disaster-create-compile-command (use-compile-db use-meson make-root cwd rel-obj obj-file proj-root rel-file file)
  "Create the actual compile command.
- USE-COMPILE-DB: non NIL to use the compiler found in
compile_commands.json,generated by CMake or Bear for example,
NIL to use Make or default compiler options,
- USE-MESON: non NIL to use the compiler found in
Meson-generated compile_commands.json,
NIL to use Make or default compiler options,
- MAKE-ROOT: path to build root,
- CWD: path to current source file,
- REL-OBJ: path to object file (relative to project root),
- OBJ-FILE: full path to object file (build root!)
- PROJ-ROOT: path to project root, REL-FILE FILE."
  (cond
   (use-meson
    (disaster-create-compile-command-meson make-root cwd rel-obj obj-file proj-root rel-file))
   (use-compile-db
    (disaster-create-compile-command-compile-db make-root cwd rel-obj obj-file proj-root rel-file))
   (t
    (disaster-create-compile-command-make make-root cwd rel-obj obj-file proj-root rel-file file))))

;;;###autoload
(defun disaster (&optional file line)
  "Show assembly code for current line of C/C++ file.

Here's the logic path it follows:

- Is there a complile_commands.json in this directory? Get the object file
  name for the current file, and run it associated command.
- Is there a Makefile in this directory? Run `make bufname.o`.
- Or is there a Makefile in a parent directory? Run `make -C .. bufname.o`.
- Or is this a C file? Run `cc -g -c -o bufname.o bufname.c`
- Or is this a C++ file? Run `c++ -g -c -o bufname.o bufname.c`
- If build failed, display errors in compile-mode.
- Run objdump inside a new window while maintaining focus.
- Jump to line matching current line.

If FILE and LINE are not specified, the current editing location
is used."
  (interactive)
  (save-buffer)
  (let* ((file (or file (buffer-file-name)))
         (filename (file-name-nondirectory file))
         (line (or line (line-number-at-pos)))
         (file-line (format "%s:%d" filename line))
         (makebuf (get-buffer-create disaster-buffer-compiler))
         (asmbuf (get-buffer-create disaster-buffer-assembly)))
    (if (or (string-match-p disaster-c-regexp filename)
            (string-match-p disaster-cpp-regexp filename)
            (string-match-p disaster-fortran-regexp filename))
        (let* ((cwd (file-name-directory file)) ;; path to current source file
               (proj-root (disaster-find-project-root nil file)) ;; path to project root
               (use-compile-db (file-exists-p (expand-file-name "compile_commands.json" proj-root)))
               (use-meson (file-exists-p (expand-file-name "meson.build" proj-root)))
               (make-root (disaster-find-build-root use-compile-db use-meson proj-root)) ;; path to build root
               (default-directory (cond
                                   (use-meson make-root)
                                   (use-compile-db proj-root)
                                   (t default-directory)))
               (rel-file (cond
                          ;; path to source file (relative to project root)
                          (use-meson (file-relative-name file make-root))
                          (proj-root (file-relative-name file proj-root))
                          (t file)))
               (rel-obj (concat (file-name-sans-extension rel-file) ".o")) ;; path to object file (relative to project root)
               (obj-file (expand-file-name rel-obj make-root)) ;; full path to object file (build root!)
               (cc (disaster-create-compile-command use-compile-db use-meson make-root cwd rel-obj obj-file proj-root rel-file file))
               (dump (format "%s %s" disaster-objdump (shell-quote-argument obj-file)))
               (line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

          ;; For CMake, read the object file from compile_commands.json
          (when (or use-compile-db use-meson)
            (let ((tmp (disaster-get-object-file-path-compile-db cc)))
              (when (file-exists-p (expand-file-name "CMakeLists.txt" proj-root))
                (setq cc (format "cmake --build %s --target %s" make-root tmp)))
              (setq obj-file (expand-file-name tmp make-root)
                    dump (format "%s %s" disaster-objdump (shell-quote-argument obj-file)))))

          (if (and (zerop (progn (message (format "Running: %s" cc))
                                 (shell-command cc makebuf)))
                   (file-exists-p obj-file))
              (when (zerop (progn (message (format "Running: %s" dump))
                                  (shell-command dump asmbuf)))
                (kill-buffer makebuf)
                (with-current-buffer asmbuf
                  ;; saveplace.el will prevent us from hopping to a line.
                  (setq-local save-place-mode nil)
                  ;; Call the configured mode `asm-mode' or `nasm-mode'
                  (when (fboundp disaster-assembly-mode)
                    (funcall disaster-assembly-mode))
                  (disaster--shadow-non-assembly-code))
                (let ((oldbuf (current-buffer)))
                  (switch-to-buffer-other-window asmbuf)
                  (goto-char 0)
                  (if (or (search-forward line-text nil t)
                          (search-forward file-line nil t))
                      (progn
                        (recenter)
                        (overlay-put (make-overlay (line-beginning-position) (1+ (line-end-position)))
                                     'face 'region))
                    (user-error "Couldn't find corresponding assembly line"))
                  (switch-to-buffer-other-window oldbuf)))
            (with-current-buffer makebuf
              (save-excursion
                (goto-char 0)
                (insert (concat cc "\n")))
              (compilation-mode)
              (display-buffer makebuf))))
      (user-error "Not a C, C++ or Fortran source file"))))

(defun disaster--shadow-non-assembly-code ()
  "Scans current buffer, which should be in `asm-mode'.
Uses the standard `shadow' face for lines that don't appear to contain
assembly code."
  (remove-overlays)
  (save-excursion
    (goto-char 0)
    (while (not (eobp))
      (beginning-of-line)
      (if (not (looking-at "[ \t]+[a-f0-9]+:[ \t]+"))
          (let ((eol (save-excursion (end-of-line) (point))))
            (overlay-put (make-overlay (point) eol)
                         'face 'shadow)))
      (forward-line))))

(defun disaster--find-parent-dirs (&optional file)
  "Return a list of parent directories with trailing slashes.

For example:

    (disaster--find-parent-dirs \"/home/jart/disaster-disaster.el\")
    => (\"/home/jart/\" \"/home/\" \"/\")

FILE default to `w/function buffer-file-name'."
  (let ((res nil)
        (dir (file-name-directory
              (expand-file-name (or file (buffer-file-name))))))
    (while dir
      (setq res (cons dir res)
            dir (if (string-match "/[^/]+/$" dir)
                    (substring dir 0 (+ 1 (match-beginning 0))))))
    (reverse res)))

(defun disaster--dir-has-file (dir file)
  "Return t if DIR contain FILE (or any file if FILE is a list).

For example:

    (disaster--dir-has-file \"/home/jart/\" \".bashrc\")
    (disaster--dir-has-file \"/home/jart/\" (list \".bashrc\" \".screenrc\"))"
  (let ((res nil)
        (dir (file-name-as-directory dir))
        (files (if (listp file)
                   file
                 (list file))))
    (while (and files (not res))
      (setq res (file-exists-p (expand-file-name (car files) dir))
            files (cdr files)))
    res))

(defun disaster-find-project-root (&optional looks file)
  "General-purpose Heuristic to detect bottom directory of project.

First, this will try to use `(vc-root-dir)' to guess the project
root directory, and falls back to manual check wich works by scanning
parent directories of FILE (using `disaster--find-parent-dirs') for certain
types of files like a `.projectile` file or a `Makefile` (which is less
preferred).

The canonical structure of LOOKS is a list of lists of files
to look for in each parent directory where sublists are ordered
from highest precedence to lowest.  However you may specify
LOOKS as a single string or a list of strings for your
convenience. If LOOKS is not specified, it'll default to
`disaster-project-root-files'."
  (let* ((buffer (get-file-buffer (or file (buffer-file-name))))
         (res (when buffer
                (with-current-buffer buffer
                  (when (vc-root-dir)
                    (expand-file-name (vc-root-dir))))))
         (looks (if looks
                    (if (listp looks)
                        (if (listp (car looks))
                            looks
                          (list looks))
                      (list (list looks)))
                  disaster-project-root-files))
         (parent-dirs (disaster--find-parent-dirs file)))
    (while (and looks (null res))
      (let ((parents parent-dirs))
        (while (and parents (null res))
          (setq res (when (disaster--dir-has-file (car parents) (car looks))
                      (car parents))
                parents (cdr parents))))
      (setq looks (cdr looks)))
    res))

(defun disaster-find-build-root (use-compile-db use-meson project-root)
  "Find the root of build directory.
USE-COMPILE-DB: non nil to use compile_commands.json,
PROJECT-ROOT: root directory of the project."
  (cond
   (use-meson
    (concat project-root disaster-meson-build-directory "/"))
   (use-compile-db
    (progn
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (json (json-read-file (expand-file-name "compile_commands.json" project-root))))
        (file-name-as-directory (gethash "directory" (car json))))))
   (t
    (and project-root
         (or (let (build-root
                   (funcs disaster-find-build-root-functions))
               (while (and (null build-root) funcs)
                 (setq build-root (funcall (car funcs) project-root)
                       funcs (cdr funcs)))
               (and build-root
                    (file-name-as-directory build-root)))
             project-root)))))

(provide 'disaster)

;;; disaster.el ends here
