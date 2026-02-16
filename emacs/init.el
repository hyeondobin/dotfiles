;; org mode를 설정하고 config.org를 불러오기.
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;; org, org-babel를 불러오기
(require 'org)
(require 'ob-tangle)

;; 모든 org-mode파일을 불러오기.
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))
