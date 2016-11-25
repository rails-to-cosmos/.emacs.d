;;; init-mac.el --- provide settings for osx
;;
;; Filename: init-mac.el
;; Description: provide settings for osx
;; Author: Dmitry Akatov
;; Created: <2016-11-25 Fri 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/core/init-mac.el
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(setq-default mac-option-key-is-meta nil
              mac-command-key-is-meta t
              mac-command-modifier 'meta
              mac-option-modifier nil)

(provide 'init-mac)
;;; init-mac.el ends here
