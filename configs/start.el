;;; configs/start.el -*- lexical-binding: t; -*-

;; Configs to run at startup
(require 'company)
(require 'editorconfig)
(require 'projectile)
(require 'tide)
(require 'treemacs)
(require 'undo-tree)
(require 'uniquify)
(require 'yasnippet)

;; Uniquify buffer names
(setq-hook! 'persp-mode-hook uniquify-buffer-name-style 'forward)


;; Enable editorconfig
(editorconfig-mode 1)
(add-hook 'prog-mode-hook #'editorconfig-mode-apply)
(add-hook 'yaml-mode-hook 'editorconfig-mode-apply)

;; LSP mode
(use-package lsp-mode :ensure t)

(setq lsp-completion-provider :none          ;; Disable LSP's completion provider
      lsp-eslint-package-manager "yarn"      ;; Set Yarn as package manager
      lsp-signature-render-documentation nil ;; Disable signature help
      lsp-ui-sideline-enable nil             ;; Disable LSP sideline symbols
      lsp-log-io nil)                        ;; if set to true can cause a performance hit

;; Increase the amount of data which Emacs reads from the process
(setq gc-cons-threshold (* 512 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-idle-delay 0.3)

;; Setup restclient mode
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

;; Apply fixes before saving
(add-hook 'before-save-hook 'lsp-eslint-apply-all-fixes)

;; Just doing (global-undo-tree-mode) is not working for some reason
(add-hook 'prog-mode-hook 'undo-tree-mode)
(add-hook 'yaml-mode-hook 'undo-tree-mode)

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
  (setq-local +lsp-company-backends '(company-tide))

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

;; Set Octave as default mode for .m files
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Set indentation for Java files
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode t)))
