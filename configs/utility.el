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
