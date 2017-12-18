;;; package --- Summary
;;; Commentary:
;;;
;;; My Emacs configuration using org babel

;;; Code:
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

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
 '(flymake-allowed-file-name-masks nil)
 '(flymake-error-bitmap nil)
 '(flymake-gui-warnings-enabled nil)
 '(flymake-no-changes-timeout 10)
 '(flymake-start-syntax-check-on-find-file nil)
 '(flymake-start-syntax-check-on-newline nil)
 '(flymake-warning-bitmap nil)
 '(package-selected-packages
   (quote
    (company-jedi tide sane-term multiple-cursors hasky-stack indent-tools yapfify py-isort importmagic company-anaconda anaconda-mode sublimity smooth-scrolling company-ghci company-ghc ghc groovy-mode color-theme-sanityinc-tomorrow cyberpunk-theme sunburn-theme material-theme tao-theme zenburn-theme zenburn flycheck-proselint flycheck-vale company-tern tern js2-mode hindent yaml-mode winum web-mode virtualenvwrapper use-package tox spaceline smartparens rainbow-delimiters popwin paradox ox-gfm org-plus-contrib org-mime org-bullets org-alert magit kv json-mode intero htmlize highlight-symbol gruvbox-theme elpy dired+ diff-hl deferred dash-functional counsel-projectile company-web company-statistics company-shell company-quickhelp company-auctex anzu all-the-icons)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval venv-workon "allure2")
     (eval progn
           (add-to-list
            (quote exec-path)
            (concat
             (locate-dominating-file default-directory ".dir-locals.el")
             "node_modules/.bin/")))
     (eval venv-workon "ws")
     (eval venv-workon "at-env")
     (haskell-indent-spaces . 4)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t nil)))
 '(flymake-warnline ((t nil)))
 '(ghc-face-error ((t nil))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'init)
;;; init.el ends here
