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

;; Allow list-packages to display correctly
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Load config file
(org-babel-load-file "~/.emacs.d/config.org")
