(custom-set-faces
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
 '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold))))

(require 'calfw-org)

;; (require 'calfw-cal)
;; (require 'calfw-ical)
;; (require 'calfw-howm)
;; (require 'calfw-org)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "IndianRed")  ; orgmode source
    ;; (cfw:howm-create-source "Blue")  ; howm source
    ;; (cfw:cal-create-source "Orange") ; diary source
    ;; (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
    ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
    )))

;; ;; Default setting
;; (setq cfw:fchar-junction ?+
;;       cfw:fchar-vertical-line ?|
;;       cfw:fchar-horizontal-line ?-
;;       cfw:fchar-left-junction ?+
;;       cfw:fchar-right-junction ?+
;;       cfw:fchar-top-junction ?+
;;       cfw:fchar-top-left-corner ?+
;;       cfw:fchar-top-right-corner ?+ )

;; ;; Unicode characters
;; (setq cfw:fchar-junction ?╋
;;       cfw:fchar-vertical-line ?┃
;;       cfw:fchar-horizontal-line ?━
;;       cfw:fchar-left-junction ?┣
;;       cfw:fchar-right-junction ?┫
;;       cfw:fchar-top-junction ?┯
;;       cfw:fchar-top-left-corner ?┏
;;       cfw:fchar-top-right-corner ?┓)

;; ;; Another unicode chars
;; (setq cfw:fchar-junction ?╬
;;       cfw:fchar-vertical-line ?║
;;       cfw:fchar-horizontal-line ?═
;;       cfw:fchar-left-junction ?╠
;;       cfw:fchar-right-junction ?╣
;;       cfw:fchar-top-junction ?╦
;;       cfw:fchar-top-left-corner ?╔
;;       cfw:fchar-top-right-corner ?╗)
