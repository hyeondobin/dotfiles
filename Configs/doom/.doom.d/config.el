;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell "/usr/bin/fish")

(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . jsonc-mode))

(setq confirm-kill-emacs nil)
(setq display-line-numbers-type 'relative)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))
(set-fontset-font "fontset-default" 'hangul (font-spec :family "D2CodingLigature Nerd Font"))

(setq catppuccin-flavor 'macchiato)

(setq doom-theme 'catppuccin)

(setq org-directory "~/org/")



(setq doom-leader-key ",")
