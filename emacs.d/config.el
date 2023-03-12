(setq user-full-name "Amr Maharek")
(setq inhibit-startup-message t)
(setq frame-resize-pixelwise t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(column-number-mode)

(setq ring-bell-function 'ignore)
(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)



;; doom-themes
;; zig-mode
;; rust-mode
;; org stuff
;; evil 
;; evil collection
;; which key
;; savehist


(straight-use-package 'doom-themes)
(straight-use-package 'zig-mode)
(use-package nix-mode
  :mode "\\.nix\\'")

(straight-use-package 'rust-mode)
(straight-use-package 'org)
(straight-use-package 'magit)
(straight-use-package 'yasnippet)
(yas-global-mode 1)

;; doom themes
(load-theme 'doom-gruvbox t)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 100)
(global-hl-line-mode t)


;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 1))))


(global-display-line-numbers-mode 1)

(use-package evil
  :init (setq evil-want-C-i-jump nil)
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


(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))


;; save history 
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))


(setq org-hide-emphasis-markers t)
 (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
   :config
   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

 (use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/RoamNotes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (add-hook 'org-mode-hook #'evil-normalize-keymaps)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))


;; Keymaps
(define-key evil-motion-state-map (kbd "SPC") nil)

(define-key evil-motion-state-map (kbd "SPC f p ") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))
(define-key evil-motion-state-map (kbd "SPC .") 'find-file )
(define-key evil-motion-state-map (kbd "SPC c c") 'compile )
(global-set-key (kbd "<escape>")    'keyboard-escape-quit)
(global-set-key (kbd "C-=")         'text-scale-increase)
(global-set-key(kbd "C--")         'text-scale-decrease)
(global-set-key(kbd "C-x C-b")         'counsel-ibuffer)
(global-set-key (kbd "M-t")         'tab-new)
(global-set-key (kbd "M-e")         'tab-next)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
