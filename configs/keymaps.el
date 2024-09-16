;;; configs/keymaps.el -*- lexical-binding: t; -*-

;; Keymaps settings
(require 'centaur-tabs)
(require 'undo-tree)

;; Tabs
(global-set-key (kbd "C-<tab>") 'centaur-tabs-forward)
(global-set-key (read-kbd-macro "C-<iso-lefttab>") 'centaur-tabs-backward)

;; Productivity
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-f") '+vertico/search-symbol-at-point)
(global-set-key (kbd "C-k") 'kill-region)
(global-set-key (kbd "C-n") 'centaur-tabs--create-new-tab)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-\\") 'treemacs-find-file)
(global-set-key (kbd "C-SPC") 'company-search-candidates)
(global-set-key (kbd "C-M-d") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-S-f") '+default/search-project-for-symbol-at-point)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "<home>") 'crux-move-beginning-of-line)

;; Indent selected region
(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)

          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

(define-key prog-mode-map (kbd "<tab>") 'shift-right)
(define-key prog-mode-map (kbd "<backtab>") 'shift-left)
