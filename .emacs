;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq package-enable-at-startup nil) (package-initialize)
(setq rtags-path "/usr/local/bin")
(set 'gc-cons-threshold 100000000)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(require 'package)
(require 'clang-format)
(require 'xterm-color)
(require 'use-package)
(require 'req-package)
(require 'company)
(require 'company-c-headers)
(require 'cmake-ide)
(require 'cmake-mode)
(require 'recentf)

;; Search for the file with the style information
;; (setq clang-format-style "file")
(setq clang-format-fallback-style "Google")

(line-number-mode 1)
(column-number-mode 1 )

(add-to-list 'package-archives
 	     '("melpa" . "http://melpa.org/packages/") t)

(setq-default c-basic-offset 4)
(electric-indent-mode 0)

; Start r-tags server if it is not running
;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;; (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
(add-hook 'after-init-hook 'global-company-mode)

;; Header File Completion for C/C++
(add-to-list 'company-backends 'company-c-headers)
;; Make sure C++ standard headers are available
(add-to-list 'company-c-headers-path-system "/usr/include/c++/9/")

;; Key Binding Section
(global-set-key (kbd "<f5>") 'rtags-find-symbol-at-point)
(global-set-key (kbd "<f6>") 'rtags-find-symbol)
(global-set-key (kbd "<f12>") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)
(global-set-key [C-M-tab] 'clang-format-region)
;; Recent File Behavior
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; Recent Files Behavior
; 50 files ought to be enough.
(recentf-mode t)
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

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
 '(custom-enabled-themes '(deeper-blue))
 '(package-selected-packages
   '(ivy-posframe doom-modeline golden-ratio which-key switch-window use-package xterm-color sr-speedbar rtags multiple-cursors magit jedi flycheck-irony flycheck-clangcheck flycheck-clang-analyzer clang-format))
 '(safe-local-variable-values
   '((cmake-ide-project-dir . "/home/resistor/TradingSystem")
     (cmake-ide-build-dir . "/home/resistor/TradingSystem/debug_build"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:extend t :background "#1d2416" :foreground "brightwhite"))))
 '(magit-diff-added-highlight ((t (:extend t :background "#222f14" :foreground "#FFFFFF" :weight bold))))
 '(magit-diff-hunk-heading ((t (:extend t :background "color-55" :foreground "brightcyan"))))
 '(magit-diff-removed ((t (:extend t :background "#454040" :foreground "#ff5bb4b")))))

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; X-Term COlor
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package golden-ratio
  :ensure t
  :hook (after-init . golden-ratio-mode)
  :custom
  (golden-ratio-exclude-modes '(occur-mode)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (column-number-mode 1)
  :custom
  (doom-modeline-height 30))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-1337 t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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

;; (use-package ivy
;;   :ensure t
;;   :demand
;;   :diminish ivy-mode
;;   :bind (("C-c C-r" . ivy-resume)
;;    ("C-x B" . ivy-switch-buffer-other-window)
;;    ("C-x b" . ivy-switch-buffer))
;;   :custom
;;   (ivy-count-format "(%d/%d) ")
;;   (ivy-use-virtual-buffers t)
;;   (ivy-use-selectable-prompt t)
;;   ;; :hook
;;   ;; (ivy-mode . ivy-posframe-mode) ;; see the posframe block below
;;   :config
;;   (ivy-mode)
;;   (setq enable-recursive-minibuffers t))
;;
;; (use-package ivy-posframe
;;   :ensure t
;;   :demand t
;;   :after ivy
;;   :custom
;;   (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;;   :config
;;   (ivy-posframe-mode 1))
;;

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

;;(use-package company-lsp
;;  :ensure t
;;  :config
;;  (push 'company-lsp company-backends)
;;  (add-hook 'after-init-hook 'global-company-mode))
;;
;;(use-package lsp-mode
;;  :config
;;  (lsp-enable-which-key-integration t)
;;  :init
;;  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;  (setq lsp-keymap-prefix "C-c l")
;;  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;         (c-mode . lsp)
;;	 (c++-mode . lsp))
;;  :commands (lsp lsp-deferred))
;;
;;(use-package lsp-ui
;;  :ensure t)

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

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.33
    which-key-idle-delay 0.05)
  :diminish which-key-mode)

(use-package whitespace
  :demand
  :init
  ;; delete trailing whitespace before saving a file
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :config
  (set-face-attribute 'trailing-whitespace nil :background "indian red")
  (setq-default show-trailing-whitespace t))

(provide 'init-which-key)
