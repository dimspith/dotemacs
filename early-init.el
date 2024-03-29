;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; We handle everything slowing down startup here i.e graphical options or fonts

;; We disable gc while loading and enable it later in the config through gcmh.
(setq gc-cons-threshold most-positive-fixnum)

;; Skip the splash screen and empty the scratch buffer
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

;; Turn off the scroll bar, menu bar and the tool bar.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Set default, fixed and variable fonts
(add-to-list 'default-frame-alist '(font . "Caskaydia Cove Nerd Font 11" ))
(set-face-attribute 'default nil :family "Caskaydia Cove Nerd Font" :height 110)

;; Prevent package.el loading packages
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
