(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font" :height 120)

(global-set-key (kbd "<escape>")    'keyboard-escape-quit)
(global-set-key (kbd "C-=")         'text-scale-increase)
(global-set-key (kbd "C--")         'text-scale-decrease)
(global-set-key (kbd "M-t")         'tab-new)
(global-set-key (kbd "M-e")         'tab-next)
(global-set-key (kbd "C-c f p")         (lambda() (interactive)(find-file "~/.config/emacs/init.el")))
(global-set-key (kbd "C-c r r")         'eval-buffer)

(define-key evil-motion-state-map (kbd "SPC") nil)

(define-key evil-motion-state-map (kbd "SPC f p ") (lambda() (interactive)(find-file "~/.config/emacs/init.el")))
(define-key evil-motion-state-map (kbd "SPC .") 'find-file )
(define-key evil-motion-state-map (kbd "SPC c c") 'compile )

