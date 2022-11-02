;;; configs/tabs.el -*- lexical-binding: t; -*-

;; Centaur tabs settings
(defun my-tabbar-buffer-groups ()
  "Set buffer groups to open everything in same window."
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              (t "user"))))

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "wave"
        centaur-tabs-height 40
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        ;;centaur-tabs-buffer-groups-function 'my-tabbar-buffer-groups
        )
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
)
