;;; configs/lsp.el -*- lexical-binding: t; -*-

(require 'lsp-mode)

;; Setup LSP
(use-package lsp-mode
  :diminish "LSP"
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((js-ts-mode
           tsx-ts-mode
           typescript-ts-mode) . lsp-deferred))
  :custom
  ;; core
  (lsp-auto-configure t)                                     ; Auto configure `lsp-mode' main features
  (lsp-eldoc-enable-hover t)                                 ; Display hover info when it is present
  (lsp-eldoc-render-all nil)                                 ; Display all of the info returned by `textDocument/hover'
  (lsp-enable-dap-auto-configure t)                          ; Enable `dap-auto-configure-mode'
  (lsp-enable-file-watchers nil)                             ; Disable watch the files in the workspace
  (lsp-enable-folding t)                                     ; Enable code folding support
  (lsp-enable-imenu t)                                       ; Enable `imenu' integration when server provides
  (lsp-enable-indentation nil)                               ; Indent regions using the file formatting functionality provided by the language server
  (lsp-enable-links nil)                                     ; All references to links in a file will be made clickable
  (lsp-enable-on-type-formatting nil)                        ; Disable `textDocument/onTypeFormatting' integration
  (lsp-enable-suggest-server-download t)                     ; Enable server downloading suggestions
  (lsp-enable-symbol-highlighting t)                         ; Highlight references of the symbol at point
  (lsp-enable-text-document-color nil)                       ; Enable `textDocument/documentColor' integration
  (lsp-enable-xref t)                                        ; Enable xref integration
  (lsp-idle-delay 0.5)                                       ; Debounce interval for `after-change-function'
  (lsp-log-io nil)                                           ; Log all messages from the language server to a lsp-log buffer
  (lsp-keep-workspace-alive nil)                             ; Keep workspace alive when the last workspace buffer is closed
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ; File where session information is stored
  (lsp-signature-doc-lines 1)                                ; Limit the number of lines to show in the docs
  (lsp-signature-render-documentation t)                     ; Display signature documentation in `eldoc'

  ;; completion
  (lsp-completion-enable t)                      ; Enable `completion-at-point' integration
  (lsp-completion-enable-additional-text-edit t) ; Apply additional text edit when performing completion
  (lsp-completion-provider :none)                ; The completion backend provider (corfu)
  (lsp-completion-show-detail t)                 ; Show detail of completion candidates
  (lsp-completion-show-kind t)                   ; Show kind of completion candidates
  (lsp-completion-show-label-description t)      ; Show description of completion candidates
  (lsp-completion-sort-initial-results t)        ; Filter initial results from server
  (lsp-completion-use-last-result t)             ; Temporarily use last server result when interrupted by keyboard
  (lsp-enable-snippet t)                         ; Enable snippet completion support

  ;; diagnostics
  (lsp-diagnostics-provider :flycheck) ; Use flycheck as diagnostics provider

  ;; headerline
  (lsp-headerline-breadcrumb-enable t)               ; Enable breadcrumb on headerline
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Apply different face on the breadcrumb based on the errors
  (lsp-headerline-breadcrumb-symbol-numbers nil)     ; Label symbols with numbers on the breadcrumb

  ;; icons
  (lsp-headerline-breadcrumb-icons-enable t) ; Enable icons support for headerline-breadcrumb

  ;; lens
  (lsp-lens-enable nil) ; Enable lenses if server supports

  ;; modeline
  (lsp-modeline-code-actions-enable nil)   ; Show code actions on modeline
  (lsp-modeline-diagnostics-enable nil)    ; Show diagnostics on modeline
  (lsp-modeline-workspace-status-enable t) ; Show workspace status on modeline

  ;; semantic
  (lsp-semantic-tokens-enable nil) ; Enable support for semantic tokens

  ;; ui
  (lsp-ui-doc-use-childframe t)             ; Show object documentation at point in a child frame
  (lsp-ui-sideline-diagnostic-max-lines 20) ; Limit diagnostic messages
  (lsp-ui-sideline-show-hover nil)          ; Show hover messages in sideline

  :init
  (setq lsp-use-plists t))

;; Disable LSP completion
(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

;; Setup LSP UI
(use-package lsp-ui
  :commands (lsp-ui-doc-show lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after lsp-mode)

;; Setup LSP eslint
(use-package lsp-eslint
  :demand t
  :after lsp-mode)

(defun auto-fix-on-save ()
  "Apply eslint fixes on save if `lsp-eslint' is active."
  (when (lsp--find-workspaces-for "eslint")
    (lsp-eslint-apply-all-fixes)))

;; Apply fixes before saving
(add-hook 'before-save-hook #'auto-fix-on-save)

;; Setup LSP treemacs
(lsp-treemacs-sync-mode 1)

;; Fix icons being crazy
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))
