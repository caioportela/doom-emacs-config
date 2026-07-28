;;; configs/org.el -*- lexical-binding: t; -*-

(after! org
  ;; Keep org tables readable by truncating long lines instead of soft-wrapping.
  (setq org-startup-truncated t)

  (add-hook! 'org-mode-hook
    (setq-local truncate-lines t)
    (setq-local word-wrap nil)
    (visual-line-mode -1)))
