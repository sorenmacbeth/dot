;;; my shit

;; start the server
(server-start)
;; swap modifiers
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
;; no native fullscreen
(setq ns-use-native-fullscreen nil)
;; something about ligatures
(mac-auto-operator-composition-mode)
;; no whitespace clean-up on save
(setq prelude-clean-whitespace-on-save nil)
;; no scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; set font
(set-frame-font "Fira Code Retina-15")
;; line numbers
(global-linum-mode t)
;; indents
(setq tab-width 2
      indent-tabs-mode nil)
;; comment-or-uncomment-region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;;; use-package
(eval-when-compile
  (require 'use-package))

;;; shell
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-show-maximum-output t)
;; for zsh EXTENDED_HISTORY
(setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")
;; xterm-color
(use-package xterm-color
  :ensure t
  :config
  (progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
         (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))))
;; exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HISTFILE"))
(defun turn-on-comint-history (history-file)
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))
;; turn on history
(add-hook 'shell-mode-hook
          (lambda ()
            (turn-on-comint-history (getenv "HISTFILE"))))
;; save history on kill
(add-hook 'kill-buffer-hook #'comint-write-input-ring)
;; shell-here
(use-package shell-here
  :ensure t
  :config
  (define-key (current-global-map) "\C-c !" 'shell-here))

;;; git-gutter-fringe+
(use-package git-gutter-fringe+
  :ensure t)

;;; nyancat
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

;;; company mode
(global-company-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

;;; java
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2)
                            (add-to-list 'c-offsets-alist '(annotation-var-cont . 0))
                            (c-set-offset 'annotation-var-cont 0)))
;;; clojure
;; no rainbow delimiters in clojure and paredit instead of smartparens
(add-hook 'clojure-mode-hook (lambda ()
                               (rainbow-delimiters-mode -1)
                               (smartparens-mode -1)
                               (paredit-mode 1)))
;; eldoc in clojure buffers
(add-hook 'cider-mode-hook 'eldoc-mode)
;; no line numbers in repl
(add-hook 'cider-repl-mode-hook (lambda ()
                                  (linum-mode -1)))
;; paredit in repl
(add-hook 'cider-repl-mode-hook 'paredit-mode)
;; smartparens in repl
;; (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
;; cider configs
(setq cider-repl-history-file "~/.cider-repl-history")
;;; json
(add-hook 'json-mode-hook (lambda ()
                            (setq js-indent-level 2)))

;;; javascript
(use-package rjsx-mode
  :ensure t
  :mode ("components\\/.*\\.js\\'" . rjsx-mode))

;;; flycheck
(use-package flycheck
  :ensure t)

;;; python
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; (setq python-shell-interpreter "python3")

;;; lsp
(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-python-ms
  :ensure t
  :config
  (setq lsp-python-ms-auto-install-server t)
  (setq lsp-python-ms-python-executable-cmd "/usr/local/bin/python3")
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp) ; or lsp-deferred
                         (setq-local flycheck-checker 'python-pylint))))

;; pyenv
;; (use-package pyenv-mode
;;   :ensure t
;;   :config
;;   (pyenv-mode))

;; elpy
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   :config
;;   ;; flycheck instead of flymake
;;   (when (load "flycheck" t t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   (setq elpy-rpc-python-command "python3"))

;; anaconda-mode
;; (add-hook 'python-mode-hook (lambda ()
;;                               (anaconda-eldoc-mode 1)))
;; (eval-after-load "company"
;;   '(add-to-list 'company-backends 'company-anaconda))

;; conda
;; (use-package conda
;;   :ensure t
;;   :init
;;   (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
;;   :config
;;   (conda-env-initialize-interactive-shells)
;;   (conda-env-initialize-eshell)
;;   (conda-env-autoactivate-mode t))

;;; org
(add-hook 'org-mode-hook (lambda ()
                           (whitespace-mode -1)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)
   (ruby . t)
   (dot . t)
   (sql . t)
   (R . t)))

;;; deft
(use-package deft
  :ensure t
  :init
  (setq deft-default-extension "org"
        deft-extensions '("org" "md" "tex" "txt")
        deft-text-mode 'org-mode
        deft-directory "~/notes"n
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t)
  :config
  (add-to-list 'auto-mode-alist '("notes/.*[.]txt$" . org-mode)))

;;; sql
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (linum-mode -1)))

;;; finda
(load "~/.finda/integrations/emacs/finda.el")

;;; direnv
;; (use-package direnv
;;   :ensure t
;;   :config
;;   (direnv-mode))

;;; nix mode
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;;; docker-compose
(use-package docker-compose-mode
  :ensure t)

;;; tabnine
;; (use-package company-tabnine
;;   :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doom-themes lsp-ivy company-tabnine docker-compose-mode nix-mode dracula-theme conda elpy zop-to-char zenburn-theme yaml-mode which-key volatile-highlights use-package undo-tree super-save smartrep smartparens shell-here rjsx-mode rainbow-mode rainbow-delimiters projectile operate-on-number nyan-mode move-text magit lsp-ui json-mode imenu-anywhere hl-todo guru-mode gitignore-mode gitconfig-mode git-timemachine git-gutter-fringe+ gist geiser flycheck expand-region exec-path-from-shell elisp-slime-nav editorconfig easy-kill discover-my-major diminish diff-hl crux counsel company-lsp company-anaconda cider browse-kill-ring beacon anzu ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
