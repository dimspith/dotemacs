;######################################################################
;### This is the init file. Here we install straight.el with        ### 
;### use-package and proceed to load the configuration file         ###
;### (config.org)                                                   ###
;######################################################################

;; Bootstrap straight.el
(defvar bootstrap-version)
(setq straight-use-package-by-default t)
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

;; Install use-package
(straight-use-package 'use-package)

;; Load config file
(org-babel-load-file "~/.emacs.d/config.org")
