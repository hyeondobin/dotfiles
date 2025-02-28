;;  elpaca installer
;;; Code:
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

; enable elpaca use package 
(elpaca elpaca-use-package
  (elpaca-use-package-mode))


;; end of elpaca installer

(setq use-package-always-ensure t)

(use-package diminish)

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
    "fr" '(recentf :which-key "Files - recent")
    )
  )

;; evil
(defun dobin/evil-hook ()
  (dolist (modesList '(custom-mode
		       eshell-mode
		       git-rebase-mode
		       erc-mode
		       circe-server-mode
		       circe-chat-mode
		       circe-query-mode
		       sauron-mode
		       term-mode))
    (add-to-list 'evil-emacs-state-modes modesList)))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-numbers
  :after evil)

(use-package hydra
  :after general)
(add-hook 'elpaca-after-init-hook (lambda () (defhydra hydra-text-scale (:timeout 4)
	  ("j" text-scale-increase "in")
	  ("k" text-scale-decrease "out")
	  ("f" nil "finished" :exit t))))
(add-hook 'elpaca-after-init-hook (lambda () (defhydra hydra-window-scale (:timeout 4)
  ("j" evil-window-increase-width "less")
  ("k" evil-window-decrease-width "more")
  ("=" balance-windows "equal")
  ("f" nil "finished" :exit t))))
(add-hook 'elpaca-after-init-hook (lambda () (dobin/leader-keys
  "st" '(hydra-text-scale/body :which-key "scale text")
  "sw" '(hydra-window-scale/body :which-key "scale window"))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; disable ui
(setq inhibit-startup-message t)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)
(set-fringe-mode 10)

(setq visible-bell t)

; line numbers
(column-number-mode) ;; display column number
(global-display-line-numbers-mode t)
;; disable line numbers for some modes
(dolist (mode '(org-mode-hook 
		term-mode-hook 
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; utils
(electric-pair-mode t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

; set font
(set-face-attribute 'default nil
		    :font "JetBrainsMono NF"
		    :height 120
		    :weight 'medium)
(set-face-attribute 'variable-pitch nil
		    :font "JetBrainsMono NF"
		    :height 130
		    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
		    :font "JetBrainsMono NF"
		    :height 120
		    :weight 'medium)
;; ref: https://gitlab.com/dwt1/configuring-emacs/-/blob/main/07-the-final-touches/config.org?ref_type=heads#evil
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-12"))
(setq-default line-spacing 0.13)


; theme
(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'macchiato))
(add-hook 'elpaca-after-init-hook (lambda () (load-theme 'catppuccin :no-confirm)))

;; completion - CO(mpletion in) R(egion) FU(unction)
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-preview-current t)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :config
  (define-key corfu-map (kbd "<RET>") nil)
  ;; ref: https://github.com/minad/corfu/issues/12#issuecomment-1171033435
  (define-key corfu-map "\C-l" #'corfu-insert)
  (define-key corfu-map "\C-y" #'corfu-insert)
  (define-key corfu-map "\C-e" #'corfu-quit)
  (define-key corfu-map "\C-n" #'corfu-next)
  (define-key corfu-map "\C-p" #'corfu-previous)
  )
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

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


;; completion - ivy
(use-package ivy
  :diminish ; diminish hides ivy from modes list? on bottom 
  :bind (("C-s" . swiper)
	:map ivy-minibuffer-map
	("TAB" . ivy-alt-done)
	("C-l" . ivy-alt-done)
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
	:map ivy-switch-buffer-map
	("C-k" . ivy-previous-line)
	("C-l" . ivy-alt-done)
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
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; don't start search with ^

; command log mode
(use-package command-log-mode
  :config
  (global-command-log-mode 1)
  (clm/open-command-log-buffer))

;; helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function) ;
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
;; TODO: add describe-symbol

;; (use-package all-the-icons)

; modeline, for now following vid, TODO: find modeline or configure mine
;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-height 15))

;; git
;; (use-package magit)

;; keymaps
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

;; or use :bind section of 'use-package'  

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(warning-suppress-log-types '((elpaca core 31.0.50))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; LSP-MODE
(use-package lsp-mode
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)
(use-package lsp-nix
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred))
  ;; :mode "\\.nix\\'")

(use-package lsp-ui
  :commands lsp-ui-mode)


;; (use-package nix-ts-mode
;;   :mode "\\.nix\\'")
