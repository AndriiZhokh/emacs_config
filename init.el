;;  _______   _____ ______   ________  ________  ________      
;; |\  ___ \ |\   _ \  _   \|\   __  \|\   ____\|\   ____\     
;; \ \   __/|\ \  \\\__\ \  \ \  \|\  \ \  \___|\ \  \___|_    
;;  \ \  \_|/_\ \  \\|__| \  \ \   __  \ \  \    \ \_____  \   
;;   \ \  \_|\ \ \  \    \ \  \ \  \ \  \ \  \____\|____|\  \  
;;    \ \_______\ \__\    \ \__\ \__\ \__\ \_______\____\_\  \ 
;;     \|_______|\|__|     \|__|\|__|\|__|\|_______|\_________\
;;                                                 \|_________|
;;  ________  ________  ________   ________ ___  ________      
;; |\   ____\|\   __  \|\   ___  \|\  _____\\  \|\   ____\     
;; \ \  \___|\ \  \|\  \ \  \\ \  \ \  \__/\ \  \ \  \___|     
;;  \ \  \    \ \  \\\  \ \  \\ \  \ \   __\\ \  \ \  \  ___   
;;   \ \  \____\ \  \\\  \ \  \\ \  \ \  \_| \ \  \ \  \|\  \  
;;    \ \_______\ \_______\ \__\\ \__\ \__\   \ \__\ \_______\ 
;;     \|_______|\|_______|\|__| \|__|\|__|    \|__|\|_______| 
;;
;; Basic UI configuration ----------------------------------------------------
(defvar runemacs/default-font-size 120)
(defvar runemacs/default-font-family "Consolas")

;; Opacity for emacs
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 50))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; Disable visible scrollbar
(tool-bar-mode -1)    ; Disable toolbar
(tooltip-mode -1)     ; Disable tooltips
(set-fringe-mode 10)  ; Give some breathing room???

(menu-bar-mode -1)    ; Disable the menu bar

(setq visible-bell t) ; Set up the visible bell

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font configuration -----------------------------------------------------------

(set-face-attribute 'default nil :font runemacs/default-font-family :height runemacs/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
 
(require 'use-package)

;; set :ensure t on by default
;; it ensures that use-package will always download packages on first install or if some of them are not presented
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)


(use-package command-log-mode)

(use-package swiper
  :ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; For Windows to correctly display icons should run `all-the-icons-install-fonts` and select folder where to install
;; then click on enach file to install fonts
;; after that you can open emacs and there should be icons insted of placeholders on `doom-modeline`
;; For Linux
;; First time  you load  your configuration on a new machine, you'll need to run the following command interactively so
;; that mode line icons display correctly
;;
;; M-x all-the-icons-install-fonts
;; and uncomment next kine
;; (use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  ;; Often used themes
  ;; (load-theme 'doom-henna t)
  ;; (load-theme 'doom-nord-light t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-ayu-light t)
  ;; (load-theme 'doom-city-lights t)
  (load-theme 'doom-earl-grey t)
  (doom-themes-visual-bell-config))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind(("M-x" . counsel-M-x)
	("C-x b" . counsel-ibuffer)
	("C-x C-f" . counsel-find-file)
	:map minibuffer-local-map
	("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

 (use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  (rune/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")
   "tf" '(find-file :which-key "find file")
   "b"  '(:ignore t :which-key "buffers")
   "bs" '(switch-to-buffer :which-key "switch to buffer")
   "bk" '(kill-this-buffer :which-key "kill current buffer")))

(defun rune/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rabase-mode
		  erc-mode
		  circle-server-mode
		  circle-chat-mode
		  circle-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; First time while added evil with all configuration it doesn't work
;; It was fixed by commented all configuration lines (left only (use-package evil) and (evil-mode 1)
;; Don't know why but evil mode doesn'y work with all these config after fresh open
;; but works fine with "minimal" config

;; C-w in nornal mode will show evil key bindings
(use-package evil
  :init
  ;; (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; (setq evil-want-C-u-scroll t)
  ;; (setq evil-want-C-i-jump nil)
  ;; :hook (evil-mode . rune/evil-hook))
  :config
  (evil-mode 1)) 
  ;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)) 

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale-text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; For some reason can't install evil-magit (
;; https://github.com/emacs-evil/evil-magit
;; This package was removed from MELPA
;; And repository is no longer maintained
;; Pacakge is now a part of `evil-collection`
;; (use-package evil-magit
;;   :after magit)

;; Can't install also that package
;; Need to figure out why
;; Also need to configure a GitHub token before using this package
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; (use-package forge)

;; Org Mode Configuration ---------------------------------------------------------------
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/ -- Need to check this article
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 0)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "???"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font runemacs/default-font-family :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ???"
	org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'("~/Notes/tasks.org"
	  "~/Notes/birthdays.org"))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-tag-alist
	'((:startgroup)
	  ;; Put matually exclusive tags here
	  (:endgroup)
	  ("@errand" . ?E)
	  ("@home" . ?H)
	  ("@work" . ?W)
	  ("agenda" . ?a)
	  ("planning" . ?p)
	  ("publish" . ?P)
	  ("batch" . ?b)
	  ("note" . ?n)
	  ("idea" . ?i)
	  ("thinking" . ?t)
	  ("recurring" . ?r)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-deadline-warning-days 7)))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))
	    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

	  ("n" "Next Tasks"
	   ((todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))

	  ("W" "Work Tasks" tags-todo "+work-email")

	  ;; Low-effort next actions
	  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	   ((org-agenda-overriding-header "Low Effort Tasks")
	    (org-agenda-max-todos 20)
	    (org-agenda-files org-agenda-files)))

	  ("w" "Workflow Status"
	   ((todo "WAIT"
		  ((org-agenda-overriding-header "Waiting on External")
		   (org-agenda-files org-agenda-files)))
	    (todo "REVIEW"
	 	  ((org-agenda-overriding-header "In Review")
	 	   (org-agenda-files org-agenda-files)))
	    (todo "PLAN"
	 	  ((org-agenda-overriding-header "In Planning")
	 	   (org-agenda-todo-list-sublevels nil)
	 	   (org-agenda-files org-agenda-files)))
	    (todo "BACKLOG"
	 	  ((org-agenda-overriding-header "Project Backlog")
	 	   (org-agenda-todo-list-sublevels nil)
	 	   (org-agenda-files org-agenda-files)))
	    (todo "READY"
	 	  ((org-agenda-overriding-header "Ready for Work")
	 	   (org-agenda-files org-agenda-files)))
	    (todo "ACTIVE"
	 	  ((org-agenda-overriding-header "Active Projects")
	 	   (org-agenda-files org-agenda-files)))
	    (todo "COMPLETED"
	 	  ((org-agenda-overriding-header "Completed Projects")
	 	   (org-agenda-files org-agenda-files)))
	    (todo "CANC"
		  ((org-agenda-overriding-header "Cancelled Projects")
		   (org-agenda-files org-agenda-files)))))))

  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("???" "???" "???" "???" "???" "???" "???")))

;; Open org mode link to other file in the same window
;; (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))





;; Some code that was generated automaticaly
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" default))
 '(package-selected-packages
   '(visual-fill-column org-bullets forge evil-magit magit counsel-projectile projectile hydra evil-collection evil general helpful doom-themes counsel swiper ivy-rich which-key rainbow-delimiters doom-modeline ivy use-package command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

