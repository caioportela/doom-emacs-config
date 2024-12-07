;;; configs/start.el -*- lexical-binding: t; -*-

(require 'editorconfig)
(require 'uniquify)

;; Enable editorconfig
(editorconfig-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Uniquify buffer names
(setq-hook! 'persp-mode-hook uniquify-buffer-name-style "forward")

;; Setup corfu
(use-package corfu
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-preview-current nil)    ; Disable current candidate preview

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; Setup flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)))
