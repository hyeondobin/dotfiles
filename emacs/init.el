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

(use-package emacs
    :demand t
    :ensure nil
    :init
    (setopt enable-recursive-minibuffers t)
    (setopt backup-by-copying t)
    (setopt sentence-end-double-space nil)
    (setopt frame-inhibit-implied-resize t) ;; tiling window manager에서는 필요가 없음
    (setopt show-trailing-whitespace t)
    (setopt user-full-name "Jehui Lee")
    (setopt user-mail-address "jayli2558@gmail.com")

    (defalias 'yes-or-no-p 'y-or-n-p)

    (setopt indent-tabs-mode nil)
    (setopt tab-width 4)
    (setopt lisp-indent-offset 4)

    (setopt help-window-select t)
    (setopt create-lockfiles nil)
    (setopt backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))

    (set-charset-priority 'unicode) ;; utf-8 everywhere
    (set-language-environment "Korean")
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)

    (global-set-key (kbd "<escape>") 'dobin/keyboard-quit) ;; ESC를 메타 프리픽스로 사용하지 않는다.

    (setopt custom-file (make-temp-file "")) ; temp file을 place holder로 사용
    (setopt custom-safe-themes t) ; mark all themes as safe
    (setopt enable-local-variables :all) ; fix =defvar= warnings

    (setopt delete-by-moving-to-trash t)

    (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)) ; elisp 컴파일 시의 경고 메세지 줄이기. 
    (setopt native-comp-async-report-warnings-errors nil)
    (setopt load-prefer-newer t)

    (show-paren-mode t)
    (visual-line-mode t)
    )

(use-package electric
    :demand t
    :ensure nil
    :init
    (electric-pair-mode t)
    (setopt electric-pair-preserve-balance t))

(use-package ediff
    :demand t
    :ensure nil)

(use-package evil
    :ensure t
    :demand t
    :custom
    (evil-cross-lines t)
    (evil-highlight-closing-paren-at-point-states '(not emacs replace))
    (evil-respect-visual-line-mode t) ; 보이는 그대로 라인 판정
    (evil-search-module 'isearch)

    (evil-undo-system 'undo-redo) ; emacs 28부터 있는 undo-redo를 사용하여 redo활성화

    ;; 이동 관련
    (evil-want-C-i-jump t)
    (evil-want-C-d-scroll t)
    (evil-want-C-u-scroll t)

    (evil-split-window-below t) ; split으로 생긴 창을 아래쪽으로
    (evil-vsplit-window-right t); split으로 생긴 창을 오른쪽으로

    :init
    (setopt evil-want-integration t) ; evil-collection의 필요 설정
    (setopt evil-want-keybinding nil)
    :config
    (evil-mode t)
    ;; 기본 시작을 노멀모드로 설정할 버퍼들
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    ;; insert 모드로 시작할 버퍼들
    (evil-set-initial-state 'eshell-mode 'insert)
    (evil-set-initial-state 'magit-diff-mode 'insert)
    )

(use-package evil-collection
    :after evil
    :custom
    (evil-collection-setup-minibuffer t)
    (evil-collection-want-find-usages-bindings t)
    :demand t
    :ensure t
    :config
    (evil-collection-init))

;; vim-commentary의 evil 버전
;; gc - evil-commentary
;; gy - evil-commentary-yank
;; s-/ evil-commentary-line
(use-package evil-commentary
    :after evil
    :ensure t
    :config
    (evil-commentary-mode))

;; surrounding 관련 모션 추가
;; surrounding 추가
;; V -> S or gS
;; N -> ys or yS
;; surrounding 변경
;; cs<old><new>
;; surrounding 제거
;; ds
(use-package evil-surround
    :after evil
    :ensure t
    :config
    (global-evil-surround-mode t))

;; evil motion에 대한 visual 힌트를 제공
(use-package evil-goggles
    :ensure t
    :config
    (evil-goggles-mode)
    (evil-goggles-use-diff-faces))

(use-package evil-owl
    :ensure t
    :diminish ""
                                        ;:config
    )

(use-package general
    :ensure t
    :demand t
    :config
    (general-evil-setup) ;; general과 evil 연계

    ;; leader key로 'space'를 사용
    (general-create-definer dobin/leader-keys
        :states '(normal insert visual emacs)
        :keymaps 'override
        :prefix "SPC" ;; leader 설정
        :global-prefix "M-SPC") ;; insert 모드에서의 리더키

    ;; local leader key로 ','를 사용
    (general-create-definer dobiin/local-leader-keys
        :states '(normal insert visual emacs)
        :keymaps 'override
        :prefix "," ;; leader 설정
        :global-prefix "M-,") ;; insert 모드에서의 로컬 리더키

    (general-define-key
        :states 'insert
        "C-g" 'dobin/keyboard-quit)

    (general-define-key
        :keymaps '(minibuffer-mode-map minibuffer-local-map)
        :states '(normal insert emacs)
        "ESC" 'dobin/keyboard-quit)

    (general-unbind
        "C-x C-r" ;; find-file read only 비활성화
        "C-x C-z" ;; suspend frame 비활성화
        "C-x C-d" ;; list directory 비활성화
        "<mouse-2>") ;; 마우스 휠 클릭으로 붙여넣기 비활성화

    (dobin/leader-keys
        "SPC" '(execute-extended-command :wk "command 실행") ;; M-x 대체
        "TAB" '(:keymap tab-prefix-map :wk "[t]ab") ;; tab 단축키 리맵
        "w" '(save-buffer :wk ":[w]rite")) ;; 빠른 저장 
    (general-define-key
        "M-w" 'dobin/save-and-kill-buffer)

    ;; help
    (dobin/leader-keys
        "h" '(:ignore t :wk "[h]elp"))

    ;; file
    (dobin/leader-keys
        "f" '(:ignore t :wk "[f]ile")
        "fc" '(:ignore t :wk "[f]ile -> [c]onfig")
        "fci" '((lambda () (interactive) (find-file user-init-file)) :wk "[f]ile [c]onfig [i]nit file")
        "fcr" '(dobin/restart-server :wk "[f]ile -> [c]onfig -> [r]eload")
        "ff" '(find-file :wk "[f]ind [f]ile")
        "fs" '(save-buffer :wk "[f]ile [s]ave")
        "fr" '(recentf :wk "[f]ile [r]ecent"))

    ;; buffer
    (dobin/leader-keys
        "b" '(:ignore t :wk "[b]uffer")
        "bs" '(switch-to-buffer :wk "[b]uffer [s]witch")
        "bd" '(kill-this-buffer :wk "[b]uffer [d]elete")
        "br" '(revert-buffer :wk "[b]uffer [r]eload(revert)"))

    ;; bookmark
    (dobin/leader-keys
        "m" '(:ignore t :wk "book[m]ark")
        "ms" '(bookmark-set :wk "[b]ookmark [s]et")
        "mj" '(bookmark-jump :wk "[b]ookmark [j]ump"))

    ;; universal argument
    (dobin/leader-keys
        "u" '(universal-argument :wk "[u]niversal prefix")) ;; C-u의 원래 기능을 리더키로 사용

    ;; open
    (dobin/leader-keys
        "o" '(:ignore t :wk "opne")
        "op" '(elpaca-log :wk "elpaca"))
    )

(use-package delsel
    :ensure nil
    :config
    (delete-selection-mode t)
    )

(use-package which-key
    :after evil
    :demand t
    :init
    (which-key-mode 1)
    :config
    ;; (which-key-setup-minibuffer)
    (which-key-setup-side-window-right-bottom)
    )

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

;; TODO: Embark Maginalia 
(use-package marginalia
    :ensure t
    :config
    (marginalia-mode))

(use-package embark
    :ensure t

    :config
    (general-define-key
        "C-," 'embark-act)
    (general-define-key
        :states 'insert
        :keymaps 'minibuffer-mode-map
        "M-e" 'embark-act)
    (add-to-list 'display-buffer-alist
        '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
             nil
             (window-parameters (mode-line-format . none)))))

(use-package command-log-mode
    :ensure t)

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

(defun dobin/save-and-kill-buffer ()
    "현재 버퍼를 저장하고 제거"
    (interactive)
    (save-buffer)
    (evil-quit))
(defun dobin/restart-server ()
    (interactive)
    (shell-command "systemctl --user restart emacs.service"))
