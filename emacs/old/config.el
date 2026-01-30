(defun my/nixos-p ()
  "Return t if operating system is NixOS, nil otherwise."
  (string-match-p "NixOS" (shell-command-to-string "uname -v")))

(defun my/nixos/get-emacs-build-date ()
  "Return NixOS Emacs build date."
  (string-match "--prefix.*emacs.*\\([[:digit:]]\\{8\\}\\)" system-configuration-options)
  (string-to-number (match-string 1 system-configuration-options)))

;; Run this before the elpaca.el is loaded. Before the installer in your init.el is a good spot.
(when (my/nixos-p) (setq elpaca-core-date (list (my/nixos/get-emacs-build-date))))

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; enable elpaca use package 
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)

;; (defun dobin/evil-hook
;;     (dolist (modesList '(custom-mode
;; 			 eshell-mode
;; 			 git-rebase-mode
;; 			 erc-mode
;; 			 circe-server-mode
;; 			 circe-chat-mode
;; 			 circe-query-mode
;; 			 sauron-mode
;; 			 term-mode))
;;       (add-to-list 'evil-emacs-state-modes modesList)))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode)
  ;; :hook (evil-mode . dobin/evil-hook)
  :config
  (evil-set-undo-system 'undo-redo)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'insert (kbd "<TAB>") 'up-list)
  (evil-global-set-key 'insert (kbd "<M-TAB>") (lambda () (interactive) (tempo-complete-tag) (org-edit-src-code) (evil-insert-state)))

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-numbers
  :after evil)

(use-package general ; key bindings manager? kinda
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer dobin/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix ","
			  :global-prefix "C-,")

  (dobin/leader-keys
    "w" '(save-buffer :which-key "Save buffer")
    "f" '(:ignore t :which-key "Files")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :which-key "Edit emacs config")
    "f g" '(find-grep-dired :which-key "Search for string in files in dir")
    "f r" '(counsel-recentf :which-key "Files - recent")
    "f w s" '((lambda() (interactive) (save-buffer) (load-file user-init-file)) :which-key "Save and reload Config")
    )
  (dobin/leader-keys
    "g" '(:ignore t :which-key "git")
    "g s" 'magit-status
    "g d" 'magit-diff-unstaged)
  (dobin/leader-keys
    "b" '(:ignore t :which-key "buffer")
    "b d" '(kill-current-buffer :which-key "Buffer Delete")
    "b l" '(ibuffer :which-key "Buffer List")
    )
  (dobin/leader-keys
    "r" '(:ignore t :which-key "Reload")
    "r c" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :which-key "Reload Config")
    "r s" '(desktop-read :which-key "Reload Session")
    "r e y" '(restart-emacs :which-key "Restart Emacs YEAH")
    )
  )

(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin :no-confirm))

(electric-pair-mode 1)
(electric-indent-mode 1)

(use-package dired-open
  :config
  (setq dired-open-extension '(("gif" . "sxiv")
			       ("jpg" . "sxiv")
			       ("png" . "sxiv")
			       ("mkv" . "mpv")
			       ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
  )

(setq inhibit-startup-message t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)

(set-fringe-mode 10)
(setq visible-bell t)
(tab-bar-mode 1)

(use-package eshell-toggle)

(use-package vterm
  :config
  (setq shell-file-name "/bin/sh"
	vterm-max-scrollback 5000))

(use-package ivy
  :diminish ; diminish hides ivy from modes list? on bottom 
  :bind (("C-s" . swiper)
	:map ivy-switch-buffer-map
	("C-d" . ivy-switch-buffer-kill)
	:map ivy-reverse-i-search-map
	("C-k" . ivy-preivous-line)
	("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

;; TODO: check repo
(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :config
  )

; idk what it exactly does. at least improved find files function.
; TODO: maybe read docs?
(use-package swiper)
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	   ("C-M-j" . ibuffer)
	 ("C-x b" . counsel-Ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))

  :config
  (setq ivy-initial-inputs-alist nil)) ; don't start search with ^

(require 'org-tempo)
(tempo-define-template "Emacs-Lisp"
				   '("#+begin_src emacs-lisp" n p n
			 "#+end_src")
				   "<el"
				   "Insert a Emacs Lisp code block"
			   'org-tempo-tags)
(add-hook 'org-mode-hook (lambda ()
			   (setq-local electric-pair-inhibit-predicate
				       `(lambda (c)
					  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(define-key org-mode-map (kbd "C-c '") '("Org enter code block" . (lambda()(interactive)(org-edit-special)(evil-insert-state))))
(define-key org-mode-map (kbd "C-c C-'") '("Org enter code block" . (lambda()(interactive)(org-edit-special)(evil-insert-state))))
(define-key org-src-mode-map (kbd "C-c '") '("Org exit code block" . (lambda ()(interactive)(org-edit-src-exit)(evil-normal-state))))
(define-key org-src-mode-map (kbd "C-c C-'") '("Org exit code block" . (lambda ()(interactive)(org-edit-src-exit)(evil-normal-state))))

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package which-key
:init (which-key-mode)
:config
(setq which-key-idle-delay 0.2))

(set-face-attribute 'default nil
:font "JetBrainsMono NF"
:height 120
:weight 'medium)
(setq-default line-spacing 0.13)

(use-package diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq backup-directory-alist '((".*" . "~/repo/dotfiles/emacs/backups")))

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(column-number-mode)

(dolist (mode '(; org-mode-hook 
		term-mode-hook 
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package dashboard
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dahshboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "EMACS")
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 5)
			  (agenda . 5)
			  (bookmarks . 3)
			  (projects . 3)
			  (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package hydra
  :after general)
(add-hook 'elpaca-after-init-hook (lambda () (defhydra hydra-text-scale (:timeout 4)
	  ("j" text-scale-increase "in")
	  ("k" text-scale-decrease "out")
	  ("f" nil "finished" :exit t))))
(add-hook 'elpaca-after-init-hook (lambda () (defhydra hydra-window-scale (:timeout 4)
  ("j" evil-window-increase-width "more")
  ("k" evil-window-decrease-width "less")
  ("=" balance-windows "equal")
  ("f" nil "finished" :exit t))))
(add-hook 'elpaca-after-init-hook (lambda () (dobin/leader-keys  
  "s" '(:ignore t :which-key "Scale")
  "st" '(hydra-text-scale/body :which-key "scale text")
  "sw" '(hydra-window-scale/body :which-key "scale window"))))

(add-hook 'after-init-hook
	  (lambda ()
	    (run-with-timer 1 nil (lambda()
				    (desktop-save-mode 1)))))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function) ;
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package magit)

(use-package lsp-mode
:init 
(setq lsp-keymap-prefix "C-c l")
:commands lsp)

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :mode "\\.nix\\'")
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
		    :major-modes '(nix-mode)
		    :priority 0
		    :server-id 'nixd)))
(use-package lsp-ui
  :commands lsp-ui-mode)

(global-set-key [escape] 'keyboard-escape-quit)

(define-key global-map (kbd "C-l") nil)

(set-language-environment "UTF-8")

(use-package company
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 1)
  (company-show-numbers t)
  (company-tooltip-align-annonations 't)
  (global-company-mode t)
  :config
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-l") #'company-complete)
  )

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode)
  )
