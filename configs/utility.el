;;; configs/utility.el -*- lexical-binding: t; -*-

;; Utility functions

(defun generate-uuid ()
  "Insert a UUID v4 at point using the uuidgen command."
  (interactive)
  (let ((uuid (string-trim-right (shell-command-to-string "uuidgen -r"))))
    (insert uuid)))

(defun shift-text-left ()
  "Indent the current line or selected lines backwards by one level."
  (interactive)

  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) -2)
    (indent-rigidly (line-beginning-position) (line-end-position) -2)))

(defun search-treemacs-workspace-for-symbol-at-point (&optional arg)
  "Search all readable projects in the current Treemacs workspace.
  With prefix ARG, include ignored and hidden files."
  (interactive "P")
  (require 'treemacs)
  (let* ((workspace (treemacs-current-workspace))
         (roots (delete-dups
                 (delq nil
                       (mapcar (lambda (project)
                                 (let ((path (treemacs-project->path project)))
                                   (when (and path (file-directory-p path))
                                     (file-name-as-directory path))))
                               (treemacs-workspace->projects workspace)))))
         (query (or (doom-thing-at-point-or-region) "")))
    (unless roots
      (user-error "No readable Treemacs projects in the current workspace"))

    (+vertico-file-search
     :query query
     :in roots
     :all-files arg
     :prompt "Search workspace")))
