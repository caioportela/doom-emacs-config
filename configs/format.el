;;; configs/format.el -*- lexical-binding: t; -*-

;; Format on save only when the project explicitly declares formatter rules.
;; This avoids Apheleia falling back to generic defaults in repos that have not
;; opted into a formatter.

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst caio/prettier-config-files
  '(".prettierrc"
    ".prettierrc.json"
    ".prettierrc.yml"
    ".prettierrc.yaml"
    ".prettierrc.json5"
    ".prettierrc.js"
    ".prettierrc.mjs"
    ".prettierrc.cjs"
    ".prettierrc.toml"
    "prettier.config.js"
    "prettier.config.mjs"
    "prettier.config.cjs"))

(defconst caio/biome-config-files
  '("biome.json"
    "biome.jsonc"))

(defun caio/project-has-file-p (files)
  "Return non-nil when one of FILES exists above `default-directory'."
  (cl-some (lambda (file) (locate-dominating-file default-directory file))
           files))

(defun caio/package-json-has-key-p (key)
  "Return non-nil when package.json has top-level KEY."
  (when-let ((root (locate-dominating-file default-directory "package.json")))
    (let ((package-json (expand-file-name "package.json" root)))
      (when (file-readable-p package-json)
        (require 'json)
        (condition-case nil
            (let ((json-key-type 'alist)
                  (json-object-type 'alist)
                  (json-array-type 'list))
              (assq key (json-read-file package-json)))
          (error nil))))))

(defun caio/prettier-project-p ()
  "Return non-nil when the current project declares Prettier config."
  (or (caio/project-has-file-p caio/prettier-config-files)
      (caio/package-json-has-key-p 'prettier)))

(defun caio/biome-project-p ()
  "Return non-nil when the current project declares Biome config."
  (caio/project-has-file-p caio/biome-config-files))

(defun caio/clang-format-project-p ()
  "Return non-nil when the current project declares clang-format config."
  (locate-dominating-file default-directory ".clang-format"))

(defun caio/prettier-mode-p ()
  "Return non-nil when the current buffer has a Prettier formatter."
  (derived-mode-p 'js-mode
                  'js-ts-mode
                  'typescript-mode
                  'typescript-ts-mode
                  'tsx-ts-mode
                  'json-mode
                  'json-ts-mode
                  'css-mode
                  'css-ts-mode
                  'html-mode
                  'html-ts-mode
                  'yaml-mode
                  'yaml-ts-mode
                  'web-mode))

(defun caio/biome-mode-p ()
  "Return non-nil when the current buffer can be formatted by Biome."
  (derived-mode-p 'js-mode
                  'js-ts-mode
                  'typescript-mode
                  'typescript-ts-mode
                  'tsx-ts-mode
                  'json-mode
                  'json-ts-mode
                  'css-mode
                  'css-ts-mode))

(defun caio/clang-format-mode-p ()
  "Return non-nil when the current buffer can be formatted by clang-format."
  (derived-mode-p 'c-mode
                  'c-ts-mode
                  'c++-mode
                  'c++-ts-mode
                  'protobuf-mode
                  'cuda-mode
                  'cuda-ts-mode))

(defun caio/project-format-configured-for-buffer-p ()
  "Return non-nil when this buffer has an explicit project formatter."
  (or (and (caio/biome-mode-p) (caio/biome-project-p))
      (and (caio/prettier-mode-p) (caio/prettier-project-p))
      (and (caio/clang-format-mode-p) (caio/clang-format-project-p))
      (derived-mode-p 'nxml-mode)))

(add-hook! 'apheleia-inhibit-functions
  (defun caio/inhibit-format-without-project-config-h ()
    "Disable Apheleia when this buffer has no project formatter config."
    (not (caio/project-format-configured-for-buffer-p))))

(after! apheleia
  (setf (alist-get 'xmllint apheleia-formatters)
        '("xmllint" "--format" "-"))
  (setf (alist-get 'nxml-mode apheleia-mode-alist) 'xmllint)

  (add-hook! '(js-mode-hook
               js-ts-mode-hook
               typescript-mode-hook
               typescript-ts-mode-hook
               tsx-ts-mode-hook
               json-mode-hook
               json-ts-mode-hook
               css-mode-hook
               css-ts-mode-hook
               html-mode-hook
               html-ts-mode-hook
               yaml-mode-hook
               yaml-ts-mode-hook
               web-mode-hook)
    (defun caio/use-project-js-formatter-h ()
      "Prefer the formatter declared by the current JS-like project."
      (setq-local +format-with
                  (cond
                   ((and (caio/biome-mode-p) (caio/biome-project-p))
                    'biome)
                   ((and (caio/prettier-mode-p) (caio/prettier-project-p))
                    nil))))))
