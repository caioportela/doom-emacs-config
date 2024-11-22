;;; configs/keymaps.el -*- lexical-binding: t; -*-

;; Keymaps settings

(require 'centaur-tabs)
(require 'treemacs)

;; Cursor
(keymap-global-set "M-S-<up>" 'mc/mark-previous-like-this)                    ; Alt + Shift + ⬆    - Add cursor to line above
(keymap-global-set "M-S-<down>" 'mc/mark-next-like-this)                      ; Alt + Shift + ↓    - Add cursor to line below
(keymap-global-set "C-d" 'mc/mark-next-like-this-word)                        ; Ctrl + d           - Find and mark the next part of the currently active region
(keymap-global-set "C-M-d" 'mc/mark-all-like-this)                            ; Ctrl + Alt + d     - Find and mark all parts of the buffer matching active region
(keymap-global-set "C-S-d" 'crux-duplicate-current-line-or-region)            ; Ctrl + Shift + d   - Duplicates the current line
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line) ; Home               - Move point back to indentation of beginning of line

;; Editor
(keymap-global-set "C-f" '+vertico/search-symbol-at-point)                    ; Ctrl + f           - Performs a search in the current buffer for thing at point
(keymap-global-set "C-k" 'kill-region)                                        ; Ctrl + k           - Kill text between point and mark
(keymap-global-set "C-s" 'save-buffer)                                        ; Ctrl + s           - Save current buffer
(keymap-global-set "C-v" 'yank)                                               ; Ctrl + v           - Reinsert ("paste") the last stretch of killed text
(keymap-global-set "C-z" 'undo)                                               ; Ctrl + z           - Undo changes
(keymap-global-set "C-SPC" 'company-search-candidates)                        ; Ctrl + SPC         - Start searching the completion candidates
(keymap-global-set "C-S-f" '+default/search-project-for-symbol-at-point)      ; Ctrl + Shift + f   - Search current project for symbol at point
(keymap-global-set "C-S-z" 'undo-redo)                                        ; Ctrl + Shift + z   - Redo changes

;; File Manager
(keymap-global-set "C-\\" 'treemacs-find-file)                                ; Ctrl + \           - Find and focus the current file in the treemacs window

;; Indentation
(keymap-global-set "<backtab>" 'shift-text-left)                              ; Shift + Tab        - Indent current line backwards by one level

;; Tabs
(keymap-global-set "C-n" 'centaur-tabs--create-new-tab)                       ; Ctrl + n           - Create a context-aware new tab
(keymap-global-set "C-w" 'kill-this-buffer)                                   ; Ctrl + w           - Kill the current buffer
(keymap-global-set "C-<tab>" 'centaur-tabs-forward)                           ; Ctrl + Tab         - Select next available tab
(keymap-global-set "C-<iso-lefttab>" 'centaur-tabs-backward)                  ; Ctrl + Shift + Tab - Select the previous available tab
