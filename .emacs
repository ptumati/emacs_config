;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq package-enable-at-startup nil) (package-initialize)
(setq rtags-path "/usr/local/bin")
(set 'gc-cons-threshold 100000000)

(require 'package)
(require 'clang-format)
(require 'xterm-color)
(require 'use-package)
(require 'req-package)
(require 'company)
(require 'company-c-headers)
(require 'cmake-ide)
(require 'cmake-mode)

(line-number-mode 1)
(column-number-mode 1 )

(add-to-list 'package-archives
 	     '("melpa" . "http://melpa.org/packages/") t)

(setq-default c-basic-offset 4)

; Start r-tags server if it is not running
;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
(add-hook 'after-init-hook 'global-company-mode)

;; Header File Completion for C/C++
(add-to-list 'company-backends 'company-c-headers)
;; Make sure C++ standard headers are available
(add-to-list 'company-c-headers-path-system "/usr/include/c++/9/")

(global-set-key (kbd "<f5>") 'rtags-find-symbol-at-point)
(global-set-key (kbd "<f6>") 'rtags-find-symbol)
(global-set-key (kbd "<f12>") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)
(global-set-key [C-M-tab] 'clang-format-region)

;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;(defun my:ac-c-header-init()
;  (require 'auto-complete-c-headers)
;  (add-to-list 'ac-sources 'ac-source-c-headers)
;  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/6.0/include")
;  (add-to-list 'achead:include-directories '"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1")
;  (add-to-list 'achead:include-directories '"/usr/local/include"))
;
;(add-hook 'c++-mode-hook 'my:ac-c-header-init)
;(add-hook 'c-mode-hook' 'my:ac-c-header-init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages
   (quote
    (helm-lsp lsp-mode switch-window use-package xterm-color sr-speedbar rtags multiple-cursors magit jedi flycheck-irony flycheck-clangcheck flycheck-clang-analyzer clang-format)))
 '(safe-local-variable-values
   (quote
    ((cmake-ide-project-dir . "/home/resistor/TradingSystem")
     (cmake-ide-build-dir . "/home/resistor/TradingSystem/debug_build")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; X-Term COlor 
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwert-shortcuts '("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

(use-package rtags
  :ensure t
  :hook (c++-mode . rtags-start-process-unless-running)
  :config (progn
	    (setq rtags-completions-enabled t
		rtags-path "/home/resistor/.emacs.d/elpa/rtags-20210313.1541"
		rtags-rc-binary-name "/usr/local/bin/rc"
		rtags-use-helm t
		rtags-rdm-binary-name "/usr/local/bin/rdm")
	    ;; Shutdown rdm when leaving emacs.
	    (add-hook 'kill-emacs-hook 'rtags-quit-rdm))
  :bind (("C-c E" . rtags-find-symbol)
  	 ("C-c e" . rtags-find-symbol-at-point)
  	 ("C-c O" . rtags-find-references)
  	 ("C-c o" . rtags-find-references-at-point)
  	 ("C-c s" . rtags-find-file)
  	 ("C-c v" . rtags-find-virtuals-at-point)
  	 ("C-c F" . rtags-fixit)
  	 ("C-c f" . rtags-location-stack-forward)
  	 ("C-c b" . rtags-location-stack-back)
  	 ("C-c n" . rtags-next-match)
  	 ("C-c p" . rtags-previous-match)
  	 ("C-c P" . rtags-preprocess-file)
  	 ("C-c R" . rtags-rename-symbol)
  	 ("C-c x" . rtags-show-rtags-buffer)
  	 ("C-c T" . rtags-print-symbol-info)
  	 ("C-c t" . rtags-symbol-type)
  	 ("C-c I" . rtags-include-file)
  	 ("C-c i" . rtags-get-include-file-for-symbol)))


(use-package lsp-mode
  :config
  (lsp-enable-which-key-integration t)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
	 (c++-mode . lsp))
  :commands (lsp lsp-deferred))

;; TODO: Has no coloring! How can I get coloring?
(req-package helm-rtags
  :require helm rtags
  :config
  (progn
    (setq rtags-display-result-backend 'helm)
    ))

;; Use rtags for auto-completion.
(req-package company-rtags
  :require company rtags
  :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)
    ))

;; Live code checking.
(req-package flycheck-rtags
  :require flycheck rtags
  :config
  (progn
    ;; ensure that we use only rtags checking
    ;; https://github.com/Andersbakken/rtags#optional-1
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.
      )
    (add-hook 'c-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
    ))

(cmake-ide-setup)
