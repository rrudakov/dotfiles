;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; My Emacs configuration using org babel

;;; Code:
(require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
;;   (add-to-list 'package-archives (cons "melpa" url) t))

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(add-to-list
 'package-archives
 '("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/") t)

(setq gc-cons-threshold (* 16 1024 1024)
      gc-cons-percentage 0.1)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/custom/")

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
