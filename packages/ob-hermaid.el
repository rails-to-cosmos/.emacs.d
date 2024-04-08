;;; ob-hermaid.el --- org-babel support for hermaid evaluation

;; Copyright (C) 2018 Alexei Nunez

;; Author: Alexei Nunez <alexeirnunez@gmail.com>
;; URL: https://github.com/arnm/ob-hermaid
;; Package-Version: 20200320.1504
;; Package-Commit: cca09b64eff689d8bb15a77de9d4c7fe9845a1f9
;; Keywords: lisp
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
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

;; Org-Babel support for evaluating hermaid diagrams.

;;; Requirements:

;; hermaid.cli | https://github.com/hermaidjs/hermaid.cli

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:hermaid
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluatiing a hermaid source block.")

(defcustom ob-hermaid-cli-path nil
  "Path to hermaid.cli executable."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:hermaid (body params)
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "hermaid requires a \":file\" header argument")))
	 (theme (cdr (assoc :theme params)))
	 (width (cdr (assoc :width params)))
	 (height (cdr (assoc :height params)))
	 (background-color (cdr (assoc :background-color params)))
	 (hermaid-config-file (cdr (assoc :hermaid-config-file params)))
	 (css-file (cdr (assoc :css-file params)))
	 (pupeteer-config-file (cdr (assoc :pupeteer-config-file params)))
         (temp-file (org-babel-temp-file "hermaid-"))
         (mmdc (or ob-hermaid-cli-path
                   (executable-find "mmdc")
                   (error "`ob-hermaid-cli-path' is not set and mmdc is not in `exec-path'")))
         (cmd (concat (shell-quote-argument (expand-file-name mmdc))
                      " -i " (org-babel-process-file-name temp-file)
                      " -o " (org-babel-process-file-name out-file)
		      (when theme
			(concat " -t " theme))
		      (when background-color
			(concat " -b " background-color))
		      (when width
			(concat " -w " width))
		      (when height
			(concat " -H " height))
		      (when hermaid-config-file
			(concat " -c " (org-babel-process-file-name hermaid-config-file)))
		      (when css-file
			(concat " -C " (org-babel-process-file-name css-file)))
                      (when pupeteer-config-file
                        (concat " -p " (org-babel-process-file-name pupeteer-config-file))))))
    (unless (file-executable-p mmdc)
      ;; cannot happen with `executable-find', so we complain about
      ;; `ob-hermaid-cli-path'
      (error "Cannot find or execute %s, please check `ob-hermaid-cli-path'" mmdc))
    (with-temp-file out-file
      (insert (format "<html lang=\"en\">
  <head><meta charset=\"utf-8\" /></head>
  <body>
    <div class=\"mermaid\">
%s
    </div>

    <script>
      var config = {
          startOnLoad:true,
          flowchart:{
              useMaxWidth:true,
              htmlLabels:true,
              curve:'cardinal',
          },
          securityLevel:'loose',
      };

      mermaid.initialize(config);
    </script>

    <script src=\"https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js\"></script>
    <script>mermaid.initialize({ startOnLoad: true });</script>
  </body>
</html>
" body)))
    ;; (message "%s" cmd)
    ;; (org-babel-eval cmd "")
    nil))

(provide 'ob-hermaid)

;;; ob-hermaid.el ends here
