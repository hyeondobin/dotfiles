;; This configuration uses org-babel for documentation and code
;; folding. This is the bootstrap file; it does the bare minimum
;; before converting and bytecompiling the main configuration file.

;; Load Elpa sources and initialize.  This is needed for later
;; configurations of packages.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil)

(let ((gc-cons-threshold (* 50 1000 1000))
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil))

  (package-initialize)

  ;; use package needs to be installed before bytecompiling the
  ;; org-babel config file.
  (unless (package-installed-p 'use-package)
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

  (require 'use-package)
  (setq use-package-always-defer t)
  (setq vc-follow-symlinks nil)

  (org-babel-load-file "~/emacs-config.org"))

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (with-current-buffer "*scratch*" (mapcar '(lambda (text) (insert text "\n")) (mapcar 'symbol-name features)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-bullets direnv nix-mode lsp-ivy lsp-ui lsp-mode paredit company-quickhelp company flycheck-pos-tip flycheck highlight-symbol which-key ivy-xref ivy-rich counsel swiper ivy projectile vterm undo-tree multiple-cursors magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
