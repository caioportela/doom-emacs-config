;;; configs/start.el -*- lexical-binding: t; -*-

(require 'editorconfig)
(require 'uniquify)

;; Enable editorconfig
(editorconfig-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set javascript indentation to 2 spaces
(setq-default js-indent-level 2)

(add-hook 'c++-mode-hook
          (lambda ()
            ;; Set C/C++ indentation to 2 spaces
            (setq-local c-basic-offset 2)

            ;; Indent C++ access specifiers by -1 level
            (c-set-offset 'access-label -2)
            (c-set-offset 'inclass 2)

            ;; Don't indent namespaces
            (c-set-offset 'innamespace 0)

            (c-set-offset 'statement-case-intro 0)
            (setq tab-width 2)))

;; Uniquify buffer names
(setq-hook! 'persp-mode-hook uniquify-buffer-name-style "forward")

;; Make shift-cursor commands select text when possible
(setq org-support-shift-select t)

;; Overwrite selected text
(delete-selection-mode t)

;; Show Emacs' built-in context menu on right-click.
(context-menu-mode 1)

;; Setup corfu
(use-package corfu
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-preview-current nil)    ; Disable current candidate preview

  :init
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (global-corfu-mode))
