;;; configs/start.el -*- lexical-binding: t; -*-

(require 'editorconfig)

;; Enable editorconfig
(editorconfig-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Uniquify buffer names
(setq-hook! 'persp-mode-hook uniquify-buffer-name-style "forward")
