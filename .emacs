
(require 'multiple-cursors)

(require 'package)
(add-to-list 'package-archives
 	     '("melpa" . "http://melpa.org/packages/") t)

; Start package.el with emacs
					; (x-focus-frame nil)
					;

; (package-initialize)
; (require 'auto-complete)
; (require 'auto-complete-config)
; (ac-config-default)
; 
; (require 'cc-mode)
; (require 'semantic)
; 
; (global-semanticdb-minor-mode 1)
; (global-semantic-idle-scheduler-mode 1)
; 
; ; Turn on Semantic
; (semantic-mode 1)
; (global-ede-mode t)
; 
; 
; (defun my:add-semantic-to-autocomplete()
;   (add-to-list 'ac-sources 'ac-source-semantic)
;   )
; (add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;Enable iEdit Mode
;(define-key global-map (kbd "C-c ;") 'iedit-mode)


(setq-default c-basic-offset 4)

; Start r-tags server if it is not running
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

(global-set-key (kbd "<f5>") 'rtags-find-symbol-at-point)
(global-set-key (kbd "<f6>") 'rtags-find-symbol)
(global-set-key (kbd "<f12>") 'compile)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages
   (quote
    (multiple-cursors jedi company-jedi company-irony flycheck-irony rtags exec-path-from-shell flycheck-clang-analyzer flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
   (require 'flycheck-clang-analyzer)
   (flycheck-clang-analyzer-setup))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


