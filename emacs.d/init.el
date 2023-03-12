
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
	  (lambda()
	    (setq gc-cons-threshold (expt 2 23))))

(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

(setq  make-backup-files nil
	   auto-save-default t)

(setq native-comp-async-report-warnings-errors nil)

(load-file "~/.emacs.d/config.el")
(load-file "~/.emacs.d/completion.el")


