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

(add-to-list
 'package-archives
 '("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/") t)

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
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (gruvbox-theme mu4e-alert zenburn-theme company-box speed-type web-mode tide yaml-mode json-mode tox yapfify company-jedi elpy virtualenvwrapper hasky-stack hindent intero org-gcal org-password-manager which-key use-package undo-tree spaceline smartparens sane-term rainbow-mode rainbow-delimiters pdf-tools paradox org-plus-contrib org-mime org-bullets org-alert ob-restclient noflet multiple-cursors magit htmlize highlight-symbol highlight-indent-guides gitignore-templates flycheck expand-region edit-server dired+ diff-hl counsel-projectile company-web company-tern company-statistics company-shell company-restclient company-quickhelp company-auctex color-theme-sanityinc-tomorrow anzu all-the-icons ace-window)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval venv-workon "bunny")
     (eval venv-workon "eotrade")
     (eval venv-workon "at-env")))))
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
