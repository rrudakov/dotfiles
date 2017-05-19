;;; package --- Summary
;;; Commentary:
;;;
;;; My Emacs configuration

;;; Code:

;;;Highlight elisp
(setq show-paren-style 'expression)
(show-paren-mode 2)

(menu-bar-mode 0)
(if window-system
    (tool-bar-mode 0))
(scroll-bar-mode 0)

(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

(set-default 'indicate-empty-lines t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight the current line
(global-hl-line-mode 1)

;;fonts
(add-to-list 'default-frame-alist '(font . "Iosevka-10"))
(set-frame-font "Iosevka-10")

(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))

;; set specific browser to open links
(setq browse-url-browser-function 'browse-url-firefox)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

;; Session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(global-set-key (kbd "<f2>") 'bs-show)

(projectile-mode)
(setq projectile-indexing-method 'native)
(add-to-list 'projectile-globally-ignored-files "*.log")

;; Install Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; Autocomplete
(require 'company)
(setq company-idle-delay 0.25)
(setq company-auto-complete nil)
(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
(add-to-list 'company-backends 'company-shell)
(add-to-list 'company-backends 'company-c-headers)
:(add-to-list 'company-backends 'company-cabal)
(add-to-list 'company-backends 'company-go)
(add-to-list 'company-backends 'company-jedi)
(add-to-list 'company-backends 'company-web-html)
(add-to-list 'company-backends 'company-web-jade)
(add-to-list 'company-backends 'company-web-slim)

(require 'company-statistics)
(company-statistics-mode)

;; (company-quickhelp-mode 1)

(require 'company-auctex)
(company-auctex-init)

;; AucTEX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(eval-after-load 'latex '(add-to-list 'LaTeX-verbatim-environments "lstlisting"))

(font-lock-add-keywords
 'latex-mode
 `((,(concat "^\\s-*\\\\\\("
             "\\(documentclass\\|\\(sub\\)?section[*]?\\)"
             "\\(\\[[^]% \t\n]*\\]\\)?{[-[:alnum:]_ ]+"
             "\\|"
             "\\(begin\\|end\\){document"
             "\\)}.*\n?")
    (0 'your-face append))))

(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))

;; Python IDE
(pyvenv-workon "at")
(when (require 'elpy nil t)
  (elpy-enable))
(setq elpy-rpc-backend "jedi")
(elpy-use-ipython)
(add-hook 'elpy-mode-hook 'yas-minor-mode-on)
(add-hook 'elpy-mode-hook 'flymake-mode-off)

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

; Autopair
(require 'smartparens-config)
(smartparens-global-mode 1)
(setq smartparens-strict-mode t)
;; (sp-pair "%" "%" :wrap "C-%")

;; JSON
(setq json-reformat:pretty-string? t)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(add-hook 'align-load-hook
          (lambda ()
	    (add-to-list 'align-rules-list
			 '(haskell-types
			   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
			   (modes quote (haskell-mode literate-haskell-mode))))))
(add-hook 'align-load-hook
          (lambda ()
	    (add-to-list 'align-rules-list
			 '(haskell-assignment
			   (regexp . "\\(\\s-+\\)=\\s-+")
			   (modes quote (haskell-mode literate-haskell-mode))))))

(add-hook 'align-load-hook
          (lambda ()
	    (add-to-list 'align-rules-list
			 '(haskell-arrows
			   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
			   (modes quote (haskell-mode literate-haskell-mode))))))

(add-hook 'align-load-hook
          (lambda ()
	    (add-to-list 'align-rules-list
			 '(haskell-left-arrows
			   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
			   (modes quote (haskell-mode literate-haskell-mode))))))


(global-set-key (kbd "M-[") 'align)

;; Web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Disable autopair in web-mode."
  (setq web-mode-enable-auto-pairing nil))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(defun sp-web-mode-is-code-context (id action context)
  "Don't remember what this ID, ACTION and CONTEXT."
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

;; Dired configuration
(require 'dired+)
(setq dired-listing-switches "--group-directories-first -alh")
(toggle-diredp-find-file-reuse-dir 1)

;; Display icons
(require 'all-the-icons)

;; Neotree
(require 'neotree)
(setq projectile-switch-project-action 'neotree-projectile-action)

(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-project-dir)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-width 45)
;;(setq neo-vc-integration '(face char))

;; Git configuration
(global-set-key (kbd "C-x g") 'magit-status)

(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; subtle diff indicators in the fringe
;; places the git gutter outside the margins.
(setq-default fringes-outside-margins t)
;; thin fringe bitmaps
(fringe-helper-define 'git-gutter-fr:added '(center repeated)
  "XXX.....")
(fringe-helper-define 'git-gutter-fr:modified '(center repeated)
  "XXX.....")
(fringe-helper-define 'git-gutter-fr:deleted 'bottom
  "X......."
  "XX......"
  "XXX....."
  "XXXX....")

(setq flycheck-indication-mode 'right-fringe)

(define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "<f5>") 'org-clock-goto)
(global-set-key (kbd "<f6>") 'org-clock-in)
(global-set-key (kbd "<f7>") 'org-clock-out)
(global-set-key "\C-cm" 'org-agenda-month-view)

(setq org-startup-indented t)
(setq org-cycle-separator-lines 0)
(setq org-tags-column -100)
(setq org-hide-leading-stars t)
(setq org-hide-leading-stars-before-indent-mode t)

(setq org-agenda-files (quote ("~/Org")))
(setq org-directory "~/Org")
(setq org-enforce-todo-dependencies t)

(setq org-tag-alist '((:startgroup . nil)
		      ("@critical" . ?C)
		      ("@medium" . ?M)
		      ("@low" . ?L)
		      (:endgroup . nil)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROGRESS(p!)" "PAUSED(h@/!)" "|" "DONE(d!)")))

(setq org-todo-keyword-faces
      '(
;	("TODO" . org-warning)
	("PROGRESS" :foreground "yellow" :weight bold)
	("PAUSED" :foreground "cyan" :weight bold)
;	("DONE" :foreground "forest green" :weight bold)
	)
      )

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-log-into-drawer t)
(setq org-log-done 'time)

(defadvice org-clock-in (after wicked activate)
  "Set this task's status to 'PROGRESS'."
  (org-todo "PROGRESS"))

(add-hook 'org-clock-out-hook 'org-todo)

(require 'org-alert)
(setq org-alert-enable t)
(setq alert-default-style 'libnotify)

(setq tramp-default-method "ssh")

(setq default-input-method "russian-computer")

(zerodark-setup-modeline-format)

;; Doom theme
(require 'doom-themes)

;;; Settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bolding are universally disabled
      doom-themes-enable-italic t  ; if nil, italics are universally disabled

      ;; doom-one specific settings
      doom-one-brighter-modeline nil
      doom-one-brighter-comments nil
      
      doom-neotree-file-icons t
      doom-neotree-enable-type-colors t)

;; Load the theme (doom-one, doom-dark, etc.)
(load-theme 'doom-one t)

;;; OPTIONAL
;; brighter source buffers (that represent files)
(add-hook 'find-file-hook #'doom-buffer-mode-maybe)
;; ...if you use auto-revert-mode
(add-hook 'after-revert-hook #'doom-buffer-mode-maybe)
;; And you can brighten other buffers (unconditionally) with:
(add-hook 'ediff-prepare-buffer-hook #'doom-buffer-mode)

;; brighter minibuffer when active
(add-hook 'minibuffer-setup-hook #'doom-brighten-minibuffer)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Enable nlinum line highlighting
(doom-themes-nlinum-config)   ; requires nlinum and hl-line-mode

;; Doom org-mode
(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(window-numbering-mode)

;; Tox
(setq tox-runner 'py.test)
(global-set-key "\C-ct" 'tox-current-class)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq paradox-github-token "619e27814936e9fe86b3394f4aca33f5f7dd9b91")

(global-anzu-mode +1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (anzu elpy traad git-gutter-fringe paradox rainbow-delimiters session tox ini-mode window-numbering use-package zerodark-theme yaml-mode web-mode spaceline-all-the-icons smartparens slack projectile org-alert org nlinum neotree intero flycheck-color-mode-line flx-ido doom-themes dired+ company-web company-statistics company-shell company-auctex)))
 '(paradox-automatically-star t)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(session-use-package t nil (session)))

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
