;;; configs/treemacs.el -*- lexical-binding: t; -*-

(use-package treemacs
  :ensure t

  :config
  (progn
    (setq treemacs-eldoc-display 'simple     ; Enables eldoc display of the file path at point
          treemacs-expand-after-init nil     ; Expand the first project after treemacs is first initialised
          treemacs-hide-dot-git-directory t  ; Indicates whether the .git directory should be hidden
          treemacs-indentation 2             ; The number of spaces or pixels each level is indented in the file tree
          treemacs-position 'left)           ; Position of treemacs buffer

    (treemacs-fringe-indicator-mode nil)) ; Treemacs-Fringe-Indicator mode

  :bind
  (:map global-map
        ("M-0" . treemacs-select-window)
        ("<f9>" . treemacs)))
