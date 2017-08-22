;;; package --- Summary
;;; Commentary:
;;;
;;; My Emacs configuration using org babel

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package org
  :ensure org-plus-contrib)

(setq vc-follow-symlinks "t")
(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-sane-defaults)))
 '(elpy-rpc-timeout 10)
 '(package-selected-packages
   (quote
    (yaml-mode winum web-mode virtualenvwrapper use-package tox spaceline smartparens rainbow-delimiters popwin paradox ox-gfm org-plus-contrib org-mime org-bullets org-alert magit kv json-mode intero htmlize highlight-symbol gruvbox-theme elpy dired+ diff-hl deferred dash-functional counsel-projectile company-web company-statistics company-shell company-quickhelp company-auctex anzu all-the-icons)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval venv-workon "at-env")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'init)
;;; init.el ends here
