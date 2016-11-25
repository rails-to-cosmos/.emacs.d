(use-package hippie-expand
  :config (progn
            (setq-default hippie-expand-try-functions-list
                  '(try-expand-dabbrev
                    try-expand-dabbrev-all-buffers
                    try-expand-dabbrev-from-kill
                    try-complete-file-name-partially
                    try-complete-file-name
                    try-expand-all-abbrevs
                    try-expand-list
                    try-expand-line
                    try-complete-lisp-symbol-partially
                    try-complete-lisp-symbol))))

;; (use-package autocomplete
;;   :config (progn
;;             (setq read-file-name-completion-ignore-case t
;;                   read-buffer-completion-ignore-case t)
;;             (mapc (lambda (x)
;;                     (add-to-list 'completion-ignored-extensions x))
;;                   '(".tramp_history" ".$$$" ".000" ".a" ".a26" ".a78" ".acn" ".acr" ".agdai" ".aif" ".alg" ".ali" ".aliases" ".annot" ".ap_" ".api" ".api-txt" ".apk" ".app" ".aps" ".autosave" ".aux" ".auxlock" ".avi" ".azurePubxml" ".bak" ".bbl" ".bcf" ".bck" ".beam" ".beams" ".bim.layout" ".bin" ".blg" ".booproj" ".bowerrc" ".box" ".bpi" ".bpl" ".brf" ".bs" ".build.csdef" ".byte" ".cachefile" ".c_date" ".cfg" ".cfgc" ".cgo1.go" ".cgo2.c" ".chi" ".chs.h" ".class" ".cma" ".cmi" ".cmo" ".cmp" ".cmx" ".cmxa" ".cmxs" ".crc" ".crs" ".csproj" ".css.map" ".cubin" ".d" ".dart.js" ".db" ".dbmdl" ".dbproj.schemaview" ".dcp" ".dcu" ".debug" ".debug.app" ".def" ".DEPLOYED" ".dex" ".dll" ".dmb" ".dotCover" ".DotSettings.user" ".dox" ".dpth" ".drc" ".drd" ".dres" ".dri" ".drl" ".dsk" ".dump" ".dvi" ".dylib" ".dyn_hi" ".dyn_o" ".ear" ".pyc" ".xls" ".DS_Store"))))

(provide 'ac)
