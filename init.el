;##############################################################
;### This is the init file. Here we install use-package and ###
;### use-package and proceed to load the configuration file ###
;### (config.org)                                           ###
;##############################################################

;; Load config file
(package-initialize)
(org-babel-load-file "~/.emacs.d/config.org")
