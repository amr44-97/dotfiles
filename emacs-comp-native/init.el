
(setq package-native-compile t)

;; Melpa ;; Org
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
(package-refresh-contents))

;;; first do  package-install use-***

(require 'use-package)
(setq use-package-always-ensure t)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(setq frame-resize-pixelwise t)

;; prevent Ugly startup 

(setq inhibit-startup-message t)
;;
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar



;; Font Configuration ----------------------------------------------------------

(set-face-attribute 'default nil :font  "CaskaydiaCove Nerd Font" :height 134)

;; Themes
(use-package doom-themes
  :init (load-theme 'doom-one-light t))

;;;; Column number enable
(column-number-mode)
(global-display-line-numbers-mode t)


;;;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))





(add-to-list 'load-path "/home/amr/.emacs.d/emacs-libvterm")
(require 'vterm)

(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                (display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                (direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))





;;;; Company completion

(add-hook 'after-init-hook 'global-company-mode)


;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>")    'keyboard-escape-quit)
(global-set-key (kbd "C-=")         'text-scale-increase)
(global-set-key (kbd "C--")         'text-scale-decrease)
(global-set-key (kbd "M-e")         'other-window)
(global-set-key (kbd "M-t")         'evil-window-split)




(use-package counsel
  :bind
         (("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))
;; helpful ui
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; M-x all-the-icons-install-fonts
(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 22)))


;; Rainbow delimeters

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))
;;
;;;; Evil Mode
;;
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


(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
;;    "." 'counsel-find-file
  ;;  "w" '(evil-window-split 'vterm-toggle)

    "bk" 'kill-current-buffer
 ;;   "dq" 'evil-quit
    "o-" 'previous-buffer                         ))


;;
;;;;   lsp-mode-setup
;;
(use-package lsp-mode
  :ensure
  :commands lsp
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         (c++-mode . lsp)

         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")

  (lsp-eldoc-render-all t)

  (lsp-idle-delay 0.6)

  (lsp-rust-analyzer-server-display-inlay-hints t)

  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))


(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))



(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("<tab>". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))
            ;; ("<tab>". tab-indent-or-complete)
             ;;("TAB". tab-indent-or-complete))


(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


(use-package flycheck :ensure)


;; eglot -- Clangd 
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-12"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode 'company-c-headers-path-system)


 ;; Enable vertico
 (use-package vertico
      :init
      (vertico-mode))

;;;; Company make faster 
;;
;;
(setq company-backends '(company-capf
                         company-keywords
                         company-semantic
                         company-files
                         company-etags
                         company-elisp
                         company-clang
                         company-irony-c-headers
                         company-irony
                         company-jedi
         		 company-c-headers-path-system
                         company-cmake
                         company-ispell
                         company-yasnippet))



(setq company-tooltip-limit 20)
(setq company-show-numbers t)
(setq company-idle-delay 0)
;;(setq company-echo-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
;; Use tab key to cycle through suggestions.
;; ('tng' means 'tab and go')
(company-tng-configure-default)
;;
;;;;;;;;;;;;;;;;
;;(defun ede-object-system-include-path ()
;;  "Return the system include path for the current buffer."
;;  (when ede-object
;;    (ede-system-include-path ede-object)))
;;
;;(setq company-c-headers-path-system 'ede-object-system-include-path)
;;

(use-package helm-company
   :ensure t
   :after (helm company)
   :bind (("C-c C-;" . helm-company))
   :commands (helm-company)
   :init
   (define-key company-mode-map (kbd "C-;") 'helm-company)
   (define-key company-active-map (kbd "C-;") 'helm-company))

;;
;;
;;
;;

;; Nim Support

;;(add-to-list 'company-backends
;;               '(company-nim :with company-nim-builtin))


(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                ;;(display-buffer-reuse-window display-buffer-in-direction)
                ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                (direction . bottom)
                ;;(dedicated . t) ;dedicated is supported in emacs27
                (reusable-frames . visible)
                (window-height . 0.3)))


;;;; Common lisp configuration
;;
;;(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "sbcl")

(require 'rust-mode)
(add-hook 'rust-mode-hook 'eglot-ensure)


;; Nim setup 
(require 'nim-mode)

;; elpy python

;; Enable elpy
(elpy-enable)







