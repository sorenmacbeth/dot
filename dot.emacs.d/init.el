;; emacs configuration

;; splat as meta
(setq ns-command-modifier 'meta)

;; delete words under active region
(pending-delete-mode t)

;; set font
(set-frame-font "DejaVu Sans Mono-10")

;; utf-8
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")

;; slime repl
(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode-hook 'ansi-color-for-comint-mode-on)

;; set slime net coding
(setq slime-net-coding-system 'utf-8-unix)

;; shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook (setq comint-prompt-read-only t))
(add-hook 'shell-mode-hook (setq comint-scroll-to-bottom-on-input t))
(add-hook 'shell-mode-hook (setq comint-scroll-show-maximum-output t))

(package-initialize)
;; el-get
(setq el-get-sources '((:name starter-kit
                              :type elpa
                              :after (progn
                                       (remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)))
                       (:name starter-kit-lisp :type elpa)
                       (:name starter-kit-bindings :type elpa)
                       (:name starter-kit-js :type elpa)
                       (:name clojure-mode :type elpa)
                       (:name clojure-test-mode :type elpa)
                       (:name exec-path-from-shell
                              :type elpa
                              :after (progn
                                       (when (memq window-system '(mac ns))
                                         (exec-path-from-shell-initialize))))
                       (:name midje-mode :type elpa)
                       (:name shell-here
                              :type elpa
                              :after (progn
                                       (require 'shell-here)
                                       (define-key (current-global-map) "\C-c!" 'shell-here)))
                       ;; (:name nrepl-ritz
                       ;;        :type elpa
                       ;;        :after (progn
                       ;;                 (add-hook 'nrepl-interaction-mode-hook 'my-nrepl-mode-setup)
                       ;;                 (defun my-nrepl-mode-setup ()
                       ;;                   (require 'nrepl-ritz))))
                       (:name nrepl
                              :type elpa
                              :after (progn
                                       (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
                                       (add-hook 'nrepl-mode-hook 'paredit-mode)
                                       (eval-after-load "auto-complete"
                                         (add-hook 'nrepl-mode-hook 'auto-complete-mode))))
                       (:name ac-nrepl
                              :type elpa
                              :after (progn
                                       (require 'ac-nrepl)
                                       (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
                                       (eval-after-load "auto-complete"
                                         '(add-to-list 'ac-modes 'nrepl-mode))))
                       (:name deft
                              :type git
                              :url "git://jblevins.org/git/deft.git"
                              :load "deft.el"
                              :after (progn
                                       (require 'deft)
                                       (setq deft-extension "org")
                                       (setq deft-directory "~/Dropbox/notes")
                                       (setq deft-text-mode 'org-mode)))
                       (:name thrift-mode
                              :type git
                              :url "git://gist.github.com/2752706.git"
                              :after (progn
                                       (require 'thrift-mode)
                                       (add-to-list 'auto-mode-alist '("\\.thrift\\'" . thrift-mode))))
                       (:name erc-customizations
                              :type git
                              :url "git://gist.github.com/2785206.git"
                              :load "ercstuff.el")
                       (:name no-easy-keys
                              :type github
                              :pkgname "danamlund/emacs-no-easy-keys"
                              :after (progn
                                       (require 'no-easy-keys)
                                       (no-easy-keys 1)))
                       (:name ham-mode
                              :type git
                              :url "git://gist.github.com/3960982.git"
                              :load "ham-mode.el"
                              :after (progn
                                       (require 'ham-mode)))
                       (:name multiple-cursors
                              :type github
                              :pkgname "magnars/multiple-cursors.el"
                              :after (progn
                                       (require 'multiple-cursors)
                                       (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                                       (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                                       (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                                       (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
                       (:name expand-region
                              :type github
                              :pkgname "magnars/expand-region.el"
                              :after (progn
                                       (require 'expand-region)
                                       (global-set-key (kbd "C-@") 'er/expand-region)))
                       (:name vc-browse
                              :type github
                              :pkgname "sorenmacbeth/vc-browse"
                              :after (progn
                                       (require 'vc-browse)))
                       (:name scala-mode2
                              :type github
                              :pkgname "hvesalai/scala-mode2"
                              :after (progn
                                       (require 'scala-mode2)))
                       (:name auto-complete
                              :after (progn
                                       (add-hook 'clojure-mode-hook 'auto-complete-mode)))
                       (:name color-theme-solarized
                              :after (progn
                                       (load-theme 'solarized-dark t)))
                       (:name yasnippet
                              :after (progn
                                       (require 'yasnippet)
                                       (yas-global-mode 1)))))

(defun sync-packages ()
  "Synchronize packages"
  (interactive)
  (el-get 'sync '(el-get package))
  (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq my-packages (append '(color-theme-solarized
                              auto-complete
                              markdown-mode
                              yasnippet
                              auto-complete-yasnippet
                              magithub
                              haskell-mode
                              ghc-mod)
                            (mapcar 'el-get-source-name el-get-sources)))
  (el-get 'sync my-packages))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(if (require 'el-get nil t)
    (sync-packages)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)
       (setq el-get-verbose t)
       (sync-packages)))))

;; start the server
(server-start)
