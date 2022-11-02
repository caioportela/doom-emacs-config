;;; configs/keymap.el -*- lexical-binding: t; -*-

;; Keymaps settings
(require 'centaur-tabs)
(require 'treemacs)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (save-excursion (goto-char (region-beginning)) (line-beginning-position))
              end (save-excursion (goto-char (region-end)) (line-end-position)))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Productivity
(global-set-key (kbd "C-k") 'kill-region)
(global-set-key (kbd "C-n") 'centaur-tabs--create-new-tab)                 ;; Open new tab
(global-set-key (kbd "C-w") 'kill-this-buffer)                             ;; Close current tab
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)          ;; Comments
(global-set-key (kbd "C-\\") 'treemacs-find-file)                          ;; Focus current file in treemacs window
(global-set-key (kbd "C-S-d") 'crux-duplicate-current-line-or-region)      ;; Duplicate line

;; centaur-tabs
(global-set-key (read-kbd-macro "C-<iso-lefttab>") 'centaur-tabs-backward) ;; Navigate to previous tab
(global-set-key (kbd "C-<tab>") 'centaur-tabs-forward)                     ;; Navigate to next tab
