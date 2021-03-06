;;##############################################################
;;### This is the init file. Here we install straight.el and ###
;;### proceed to load the configuration file (config.org)    ###
;;##############################################################


;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)

;; Load config file
(org-babel-load-file "~/.config/emacs/config.org")
