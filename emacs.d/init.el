;;; package --- Summary
;;; Commentary:
;;;
;;; My Emacs configuration

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Dired configuration
(require 'dired+)
(setq dired-listing-switches "--group-directories-first -alh")
(toggle-diredp-find-file-reuse-dir 1)

(require 'org)
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
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-sane-defaults)))
 '(elpy-rpc-timeout 10)
 '(flymake-allowed-file-name-masks
   (quote
    (("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init nil nil)
     ("\\.xml\\'" flymake-xml-init nil nil)
     ("\\.html?\\'" flymake-xml-init nil nil)
     ("\\.cs\\'" flymake-simple-make-init nil nil)
     ("\\.p[ml]\\'" flymake-perl-init nil nil)
     ("\\.php[345]?\\'" flymake-php-init nil nil)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup nil)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup nil)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup nil)
     ("\\.tex\\'" flymake-simple-tex-init nil nil)
     ("\\.idl\\'" flymake-simple-make-init nil nil))))
 '(package-selected-packages
   (quote
    (company-shell org-bullets gitignore-mode magit company-quickhelp virtualenvwrapper epc json-mode gruvbox-theme ido-vertical-mode popwin winum spaceline diminish yaml-mode company-c-headers company-cabal company-go company-jedi solaire-mode highlight-symbol diff-hl anzu paradox rainbow-delimiters tox ini-mode window-numbering use-package web-mode spaceline-all-the-icons smartparens projectile org-alert org nlinum neotree intero flycheck-color-mode-line flx-ido doom-themes dired+ company-web)))
 '(paradox-automatically-star t)
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
 '(window-divider ((t (:foreground "#282828")))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'init)
;;; init.el ends here
