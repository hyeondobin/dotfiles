(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file "~/repo/dotfiles/emacs/customs.el")

(defvar elpaca-installer-version 0.11)
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package evil :ensure t :demand t
  :init
  (setq evil-want-C-u-scroll t
	evil-cross-lines t
	evil-split-window-below t
	evil-split-window-right t
	evil-highlight-closing-paren-at-point-states '(not emacs replace)
	evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-owl
  :ensure t
  :diminish ""
  ;:config
  )

(which-key-mode 1)

(use-package tree-sitter :ensure t :demand t :config (global-tree-sitter-mode))
(use-package tree-sitter-langs :ensure t :demand t)
(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package catppuccin-theme
  :ensure t
  :demand t
  ;; :hook (server-after-make-frame . catppuccin-reload)
  :init
  (setq catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin :no-confirm))

;; (set-frame-font "JetBrains Mono NF 18" nil t)
(add-to-list 'default-frame-alist '(font . "JetBrains Mono NF-14"))
(setq-default inhibit-startup-message t
	      use-short-answers t)
(recentf-mode t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package corfu
  :ensure t
  :hook (prog-mode . (lambda () (setq-local corfu-auto t)))
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :config
  (keymap-unset corfu-map "RET")
  )
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package cape
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles 'orderless-flex)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))
  
(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  :init
  (defun dobin/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun dobin/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless))
    (setq-local orderless-style-dispatchers (list #'dobin/orderless-dispatch-flex-first))
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))
  :hook
  (lsp-completion-mode . dobin/lsp-mode-setup-completion))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq-default display-line-numbers 'visual)

(defun dobin/keyboard-quit ()
  "Smarter version of the built-in `keyboard-quit'."
  (interactive)
  (if (active-minibuffer-window)
      (if (minibufferp)
	  (minibuffer-keyboard-quit)
	(abort-recursive-edit))
    (keyboard-quit)))
(global-set-key [remap keyboard-quit] #'dobin/keyboard-quit)
