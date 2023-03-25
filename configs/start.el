;;; configs/start.el -*- lexical-binding: t; -*-

;; Configs to run at startup
(require 'company)
(require 'projectile)
(require 'tide)
(require 'treemacs)
(require 'undo-tree)
(require 'uniquify)
(require 'yasnippet)

;; Uniquify buffer names
(toggle-uniquify-buffer-names)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)

;; LSP mode
(setq lsp-completion-provider :none)           ;; Disable LSP's completion provider
(setq lsp-eslint-package-manager "yarn")       ;; Set Yarn as package manager
(setq lsp-signature-render-documentation nil)  ;; Disable signature help
(setq lsp-ui-sideline-enable nil)              ;; Disable LSP sideline symbols

;; Apply fixes before saving
(add-hook 'before-save-hook 'lsp-eslint-apply-all-fixes)

;; Enable undo-tree.
(global-undo-tree-mode)

;; Just doing (global-undo-tree-mode) is not working for some reason
(add-hook 'prog-mode-hook 'undo-tree-mode)

;; Start hide/show minor mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Start yasnippets
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; Define a function to configure Tide mode for TypeScript files
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)

  ;; Set the sorting order for company completions
  (setq-local company-transformers '(company-sort-by-backend-importance))

  ;; Set lsp company backends to nil
  (setq-local +lsp-company-backends '(company-tide company-yasnippet))

  ;; Enable Tide completion using company backend
  (setq-local tide-completion-setup-company-backend t)

  ;; Set the project root for the Tide server
  (setq-local tide-project-root (projectile-project-root))

  ;; Enable fuzzy matching for completions
  (setq-local tide-completion-fuzzy t)

  ;; Enable case insentive candidates
  (setq-local tide-completion-ignore-case t)

  ;; Set user preferences for Tide
  (setq-local tide-user-preferences
              '(:includeCompletionsForModuleExports t
                :includeExternalModules t
                :importModuleSpecifierPreference "non-relative")))

;; Add the Tide mode configuration to the typescript-mode-hook
(add-hook 'typescript-mode-hook 'setup-tide-mode)

;; Add function to generate UUID v4
(defun insert-uuid ()
  "Insert a UUID v4 at point using the uuidgen command."
  (interactive)
  (let ((uuid (string-trim-right (shell-command-to-string "uuidgen -r"))))
    (insert uuid)))
