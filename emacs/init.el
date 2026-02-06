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
  :custom
  (evil-want-C-u-scroll t)
  (evil-cross-lines t)
  (evil-split-window-below t)
  (evil-split-window-right t)
  (evil-highlight-closing-paren-at-point-states '(not emacs replace))
  (evil-undo-system 'undo-redo)
  :init
  (setopt evil-want-integration t)
  (setopt evil-want-keybinding nil)
  :config
  (evil-define-key 'motion 'global
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-want-find-usages-bindings t)
  :demand t
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-owl
  :ensure t
  :diminish ""
  ;:config
  )

;; leader map
(defvar dobin-leader-map (make-sparse-keymap)
  "\"leader key\"를 위한 키맵")
(defvar dobin-search-map (make-sparse-keymap)
  "Search keymap")

(keymap-set evil-normal-state-map "<space>" dobin-leader-map)

(keymap-set dobin-leader-map "s" '("[S]earch" . dobin-search-map))
(keymap-set dobin-search-map "." 'recentf)

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

;; 한글 예시문
;; D2Coding Nerd Font의 Mono는 글자 폭이 잘못 설정되어 있어서 사용할 시 영어의 폭과 맞춰지기 때문에 일반 폰트로 사용해야한다.
;; https://codepractice.tistory.com/167
(defun dobin/set-korean-font (frame)
  "Set font for new frames"
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (set-fontset-font "fontset-default" 'hangul (font-spec :family "D2CodingLigature Nerd Font" :size 18)))))
(add-hook 'after-make-frame-functions 'dobin/set-korean-font)

(add-to-list 'default-frame-alist '(font . "JetBrains Mono NFM-14"))

(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

(setq-default inhibit-startup-message t
	      use-short-answers t)

(recentf-mode t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(defun dobin/elisp-mode-init ()
  "Set completion function to cape"
  (setq-local completion-at-point-functions
	      (list (cape-capf-inside-code #'cape-elisp-symbol))))
(add-hook 'emacs-lisp-mode-hook #'dobin/elisp-mode-init)

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
