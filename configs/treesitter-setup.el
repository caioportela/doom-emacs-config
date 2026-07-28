;;; configs/treesitter-setup.el -*- lexical-binding: t; -*-

(defun install-treesitter-grammars ()
  "Install tree-sitter grammars."
  (interactive)
  (dolist (grammar
           '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.23.1"))
             (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.23.2"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1" "src"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
             (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))

    (add-to-list 'treesit-language-source-alist grammar)

    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(use-package treesit
  :mode (("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :preface 'install-treesitter-grammars
  (dolist (mapping
           '((bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js2-mode . js-ts-mode)
             (typescript-mode . typescript-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (install-treesitter-grammars))
