;; emacs configuration

;; splat as meta
(setq ns-command-modifier 'meta)

;; toggle fullscreen
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;; delete words under active region
(pending-delete-mode t)

;; set font
(set-frame-font "DejaVu Sans Mono-9")

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

;; el-get

(setq el-get-sources '((:name starter-kit
                              :type elpa
                              :after (progn
                                       (remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)
                                       (add-hook 'prog-mode-hook 'auto-complete-mode)))
                       (:name starter-kit-lisp :type elpa)
                       (:name starter-kit-eshell :type elpa)
                       (:name starter-kit-bindings :type elpa)
                       (:name paredit
                              :type elpa
                              :after (progn
                                       (defun paredit-wrap-round-from-behind ()
                                         (interactive)
                                         (forward-sexp -1)
                                         (paredit-wrap-round)
                                         (insert " ")
                                         (forward-char -1))

                                       (define-key paredit-mode-map (kbd "M-)")
                                         'paredit-wrap-round-from-behind)))
                       (:name project-mode
                              :type elpa
                              :after (progn
                                       (require 'project-mode)
                                       (project-load-all)))
                       (:name clojure-project-mode
                              :type elpa
                              :after (progn
                                       (require 'clojure-project-mode)))
                       (:name clojure-mode
		       	      :type elpa
		       	      :after (progn
		       		       ;; colorize slime repl
		       		       (add-hook 'slime-repl-mode-hook
		       				 (defun clojure-mode-slime-font-lock ()
		       				   (let (font-lock-mode)
		       				     (clojure-mode-font-lock-setup))))
                                       (add-hook 'clojure-mode-hook 'clojure-project-mode)))
                       (:name clojure-test-mode :type elpa)
                       (:name midje-mode
                              :type elpa
                              :after (progn
                                       (add-hook 'clojure-mode-hook 'midje-mode)))
                       (:name kibit-mode
                              :type git
                              :url "https://github.com/aredington/kibit-mode.git"
                              :after (progn
                                       (require 'kibit-mode)
                                       (add-hook 'clojure-mode-hook 'kibit-mode)))
                       (:name gist :type elpa)
                       (:name magithub :type elpa)
                       (:name pastebin :type elpa)
                       (:name exec-path-from-shell
                              :type elpa
                              :after (progn
                                       (when (memq window-system '(mac ns))
                                         (exec-path-from-shell-initialize))))
                       (:name shell-here
                              :type elpa
                              :after (progn
                                       (require 'shell-here)
                                       (define-key (current-global-map) "\C-c!" 'shell-here)))
                       (:name nrepl-ritz :type elpa)
                       (:name nrepl
                              :type elpa
                              :after (progn
                                       (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
                                       (add-hook 'nrepl-mode-hook 'paredit-mode)
                                       ;; (add-hook 'nrepl-mode-hook
                                       ;;           (defun nrepl-ritz-setup ()
                                       ;;             (require 'nrepl-ritz)))
                                       ))
                       (:name ac-nrepl
                              :type elpa
                              :after (progn
                                       (require 'ac-nrepl)
                                       (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
                                       (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
                                       (eval-after-load "auto-complete"
                                         '(add-to-list 'ac-modes 'nrepl-mode))))
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
                              :type git
                              :url "git://github.com/danamlund/emacs-no-easy-keys.git"
                              :load "no-easy-keys.el"
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
                              :type git
                              :url "git://github.com/magnars/multiple-cursors.el"
                              :load "multiple-cursors.el"
                              :after (progn
                                       (require 'multiple-cursors)
                                       (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                                       (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                                       (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                                       (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))
                       (:name expand-region
                              :type git
                              :url "git://github.com/magnars/expand-region.el"
                              :load "expand-region.el"
                              :after (progn
                                       (require 'expand-region)
                                       (global-set-key (kbd "C-@") 'er/expand-region)))
                       (:name color-theme-solarized
                              :after (progn
                                       (load-theme 'solarized-dark t)))
		       (:name ac-slime
			      :after (progn
				       (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))))

(defun sync-packages ()
  "Synchronize packages"
  (interactive)
  (el-get 'sync '(el-get package))
  (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq my-packages (append '(color-theme-solarized auto-complete ac-slime) (mapcar 'el-get-source-name el-get-sources)))
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

;; paredit stuff
(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(define-key paredit-mode-map (kbd "M-)")
  'paredit-wrap-round-from-behind)

;; start the server
(server-start)
