;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; We handle everything slowing down startup here i.e graphical options or fonts

;; We disable gc while loading and enable it later in the config.
(setq gc-cons-threshold most-positive-fixnum)

;; Skip the splash screen and empty the scratch buffer
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-echo-message t)

;; Turn off the scroll bar, menu bar and the tool bar.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Set default, fixed and variable fonts
(set-face-attribute 'default nil :family "Fira Code" :height 100)
(set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 100)
(set-face-attribute 'variable-pitch nil :family "Cantarell" :height 100 :weight 'regular)
