;;; configs/claude-code-ide.el -*- lexical-binding: t; -*-

(use-package! claude-code-ide
  :bind ("C-c C-a" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))
