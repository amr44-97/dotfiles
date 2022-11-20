(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(column-number-mode)

(setq ring-bell-function 'ignore)


;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))



;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)


;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))


;; Show line numbers
(global-display-line-numbers-mode 1)

;; Theme - Font
(load-theme 'modus-vivendi t)
(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font" :height 120)


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
(package-refresh-contents))




(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs 
  projectile hydra flycheck company avy which-key  dap-mode ))

(require 'use-package)
(setq use-package-always-ensure t)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(setq frame-resize-pixelwise t)

(use-package doom-themes)


(use-package nasm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))


(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
   (setq vertico-cycle t)
  )

(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


(define-key vertico-map "?" #'minibuffer-completion-help)
(define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
(define-key vertico-map (kbd "M-TAB") #'minibuffer-scroll-down-command)



(use-package all-the-icons
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 20)))

(setq doom-modeline-time t)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Rainbow delimeters

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(use-package zig-mode)


(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'zig-mode-hook 'lsp)

(setq lsp-zig-zls-executable "/opt/zig-linux-x86_64-dev/zls")
(setq zig-zig-bin "/opt/zig-linux-x86_64-dev/zig")

(setq gc-cons-threshold (* 50 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))


;;;; Evil Mode

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (evil-mode 1)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; Magit 
(use-package magit
  :ensure t)


;;;;;;  KeyMaps


(global-set-key (kbd "<escape>")    'keyboard-escape-quit)
(global-set-key (kbd "C-=")         'text-scale-increase)
(global-set-key (kbd "C--")         'text-scale-decrease)
(global-set-key (kbd "M-t")         'tab-new)
(global-set-key (kbd "M-e")         'tab-next)
(global-set-key (kbd "C-c f p")         (lambda() (interactive)(find-file "~/.config/emacs/init.el")))

(define-key evil-motion-state-map (kbd "SPC") nil)

(define-key evil-motion-state-map (kbd "SPC f p ") (lambda() (interactive)(find-file "~/.config/emacs/init.el")))
(define-key evil-motion-state-map (kbd "SPC .") 'find-file )




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(all-the-icons-ibuffer all-the-icons-dired magit lsp-mode yasnippet lsp-treemacs projectile hydra flycheck company avy which-key dap-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
