; elpaca installer
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
					; end of elpaca installer
					(setq use-package-always-ensure t)

; enable elpaca use package 
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package general ; key bindings manager? kinda
  :config
  (general-evil-setup t)
  (general-create-definer dobin/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix ","
			  :global-prefix "C-,")
  (dobin/leader-keys
    "w" '(save-buffer :which-key "Save buffer")))

;; evil
(defun dobin/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . dobin/evil-hook)
  :config
  (evil-mode 1)
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

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
	  ("j" text-scale-increase "in")
	  ("k" text-scale-decrease "out")
	  ("f" nil "finished" :exit t))
(defhydra hydra-window-scale (:timeout 4)
  ("j" evil-window-increase-width "less")
  ("k" evil-window-decrease-width "more")
  ("=" balance-windows "equal")
  ("f" nil "finished" :exit t))
(dobin/leader-keys
  "st" '(hydra-text-scale/body :which-key "scale text")
  "sw" '(hydra-window-scale/body :which-key "scale window"))

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
(set-face-attribute 'default nil :font "JetBrainsMono NF" :height 120)

; theme
(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'macchiato))
(add-hook 'elpaca-after-init-hook (lambda () (load-theme 'catppuccin :no-confirm)))


; completion - ivy
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
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
;; TODO: add describe-symbol

(use-package all-the-icons)

; modeline, for now following vid, TODO: find modeline or configure mine
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15))

;; git
;; (use-package magit)

;; keymaps
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme)

;; or use :bind section of 'use-package'  

