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

(add-to-list
 'package-archives
 '("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/") t)

(setq gc-cons-threshold (* 16 1024 1024)
      gc-cons-percentage 0.1)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(package-initialize)


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

(when (file-exists-p custom-file)
  (load custom-file))
(provide 'init)
;;; init.el ends here
