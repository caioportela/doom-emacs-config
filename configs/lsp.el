;;; configs/lsp.el -*- lexical-binding: t; -*-

;; Doom is configured with `:tools (lsp +eglot)`, so keep this file focused on
;; Emacs' built-in LSP path instead of loading `lsp-mode` separately.

(require 'seq)
(require 'subr-x)

(defun caio/newest-nvm-typescript-language-server ()
  "Return the newest NVM typescript-language-server executable, if installed."
  (let* ((nvm-dir (or (getenv "NVM_DIR") (expand-file-name "~/.nvm")))
         (versions-dir (expand-file-name "versions/node" nvm-dir)))
    (when (file-directory-p versions-dir)
      (seq-find
       #'file-executable-p
       (mapcar (lambda (dir)
                 (expand-file-name "bin/typescript-language-server" dir))
               (sort (directory-files versions-dir t "^v[0-9]")
                     (lambda (a b)
                       (version<
                        (string-remove-prefix "v" (file-name-nondirectory b))
                        (string-remove-prefix "v" (file-name-nondirectory a))))))))))

(defun caio/prepend-exec-path (dir)
  "Prepend DIR to both `exec-path' and the PATH environment variable."
  (when (and dir (file-directory-p dir))
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))))

(when-let ((tsls (caio/newest-nvm-typescript-language-server)))
  (caio/prepend-exec-path (file-name-directory tsls)))

(after! eglot
  (setq eglot-ignored-server-capabilities
        '(:documentOnTypeFormattingProvider))

  ;; Match the old lsp-mode preference for JavaScript/TypeScript completions.
  (setq-default eglot-workspace-configuration
                '(:javascript (:suggest (:completeJSDocs :json-false))
                  :typescript (:suggest (:completeJSDocs :json-false))))

  ;; Eglot already ships the correct JS/TS language-id mapping; the NVM path
  ;; bridge above makes its default typescript-language-server command usable.
  )

(add-hook! '(js-mode-hook
             js-ts-mode-hook
             typescript-mode-hook
             typescript-ts-mode-hook
             tsx-ts-mode-hook)
  #'eglot-ensure)

(defun caio/eglot-lsp-xrefs-at-point (method capability)
  "Return Eglot xrefs at point for LSP METHOD when CAPABILITY is available."
  (when (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-server-capable)
             (eglot-server-capable capability)
             (fboundp 'eglot--lsp-xrefs-for-method))
    (eglot--lsp-xrefs-for-method method :capability capability)))

(defun caio/xref-find-definition-or-implementation-at-mouse (event)
  "Find the Eglot implementation or definition at EVENT."
  (interactive "e")
  (require 'eglot)
  (require 'xref)
  (mouse-set-point event)
  (unless (bound-and-true-p eglot--managed-mode)
    (user-error "Eglot is not active in this buffer"))
  (if-let ((xrefs (or (caio/eglot-lsp-xrefs-at-point
                       :textDocument/implementation
                       :implementationProvider)
                      (caio/eglot-lsp-xrefs-at-point
                       :textDocument/definition
                       :definitionProvider))))
      (xref--show-defs xrefs nil)
    (user-error "No LSP definition or implementation found here")))

(defun caio/context-menu-use-eglot-implementation-a (fn menu click)
  "Prefer Eglot implementations from the built-in programming context MENU."
  (let ((menu (funcall fn menu click)))
    (when (lookup-key menu [find-def])
      (define-key menu [find-def]
                  '(menu-item "Find Definition"
                              caio/xref-find-definition-or-implementation-at-mouse
                              :help "Find implementation or definition via Eglot")))
    menu))

(after! prog-mode
  (advice-add 'prog-context-menu
              :around
              #'caio/context-menu-use-eglot-implementation-a))

(after! flymake
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error))
