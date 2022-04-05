(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable toolbar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Give some breathing room???
(menu-bar-mode -1)    ; Disable the menu bar

(setq visible-bell t) ; Set up the visible bell
(set-face-attribute 'default nil :height 100)
(load-theme 'tango-dark)
