;;; package --- Summary
;;; Commentary:
;;;
;;; My Emacs configuration

;;; Code:

;;;Highlight elisp
(setq show-paren-style 'expression)
(show-paren-mode 2)

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

(add-to-list 'display-buffer-alist
             '("." nil (reusable-frames . t)))

;;;
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system        'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; perdy
(setq locale-coding-system   'utf-8)   ; please
(setq-default buffer-file-coding-system 'utf-8) ; with sugar on top

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 apropos-do-all t                 ; make `apropos' more useful
 compilation-always-kill t        ; kill compilation process before starting another
 compilation-ask-about-save nil   ; save all buffers on `compile'
 compilation-scroll-output t
 confirm-nonexistent-file-or-buffer t
 enable-recursive-minibuffers nil
 ;; idle-update-delay 2              ; update ui less often
 ;; keep the point out of the minibuffer
 minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 1000
 make-backup-files nil)

;; be quiet at startup
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq-default
 bidi-display-reordering nil ; disable bidirectional text for tiny performance boost
 blink-matching-paren nil    ; don't blink--too distracting
 cursor-in-non-selected-windows nil  ; hide cursors in other windows
 frame-inhibit-implied-resize t
 ;; remove continuation arrow on right fringe
 fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)
                              fringe-indicator-alist)
 highlight-nonselected-windows nil
 image-animate-loop t
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 max-mini-window-height 0.3
 mode-line-default-help-echo nil ; disable mode-line mouseovers
 mouse-yank-at-point t           ; middle-click paste at point, not at click
 resize-mini-windows 'grow-only  ; Minibuffer resizing
 show-help-function nil          ; hide :help-echo text
 split-width-threshold nil       ; favor horizontal splits
 uniquify-buffer-name-style 'forward
 use-dialog-box nil              ; always avoid GUI
 visible-cursor nil
 x-stretch-cursor nil
 ;; defer jit font locking slightly to [try to] improve Emacs performance
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil
 ;; `pos-tip' defaults
 pos-tip-internal-border-width 6
 pos-tip-border-width 1
 ;; no beeping or blinking please
 ring-bell-function #'ignore
 visible-bell nil)

;;; More reliable inter-window border
;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq-default window-divider-default-places t
              window-divider-default-bottom-width 0
              window-divider-default-right-width 1)
(window-divider-mode +1)

(defvar my-ui-fringe-size '4 "Default fringe width.")

(tooltip-mode -1) ; relegate tooltips to echo area only
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (setq-default line-spacing 0)
  ;; buffer name  in frame title
  ;; (setq-default frame-title-format '("DOOM Emacs"))
  ;; standardize fringe width
  (push (cons 'left-fringe  my-ui-fringe-size) default-frame-alist)
  (push (cons 'right-fringe my-ui-fringe-size) default-frame-alist))

(setq menu-bar-select-buffer-function 'switch-to-buffer)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

;; ;; Doom theme
;; (require 'doom-themes)

;; ;; Global settings (defaults)
;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t  ; if nil, italics is universally disabled

;;       ;;      doom-neotree-file-icons t
;;       ;;      doom-neotree-enable-type-colors t
;;       )

;; ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; ;; may have their own settings.
;; (load-theme 'doom-one t)

;; ;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;; ;; Enable custom neotree theme
;; ;; (doom-themes-neotree-config)

;; ;; Doom org-mode
;; (setq org-fontify-whole-heading-line t
;;       org-fontify-done-headline t
;;       org-fontify-quote-and-verse-blocks t)

(load-theme 'base16-oceanicnext t)
(require 'solaire-mode)

;; brighten buffers (that represent real files)
(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

;; ...if you use auto-revert-mode:
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; You can do similar with the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(global-set-key (kbd "<f2>") 'bs-show)

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-vertical-show-count t)

(projectile-mode)
(add-to-list 'projectile-globally-ignored-files "*.log")
(setq projectile-enable-caching t)
(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

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
(add-to-list 'company-backends 'company-cabal)
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
;; (require 'neotree)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;; (defun neotree-project-dir ()
;;     "Open NeoTree using the git root."
;;     (interactive)
;;     (let ((project-dir (projectile-project-root))
;;           (file-name (buffer-file-name)))
;;       (neotree-toggle)
;;       (if project-dir
;;           (if (neo-global--window-exists-p)
;;               (progn
;;                 (neotree-dir project-dir)
;;                 (neotree-find file-name)))
;;         (message "Could not find git project root."))))

;; (global-set-key [f8] 'neotree-project-dir)
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; (setq neo-window-width 45)
;; (setq neo-vc-integration '(face char))

;; Git configuration
(global-set-key (kbd "C-x g") 'magit-status)

;; subtle diff indicators in the fringe
;; places the git gutter outside the margins.
(setq-default fringes-outside-margins t)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(setq flycheck-indication-mode 'right-fringe)

(define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0])

(require 'highlight-symbol)

(highlight-symbol-nav-mode)

(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))

(setq highlight-symbol-idle-delay 0.2
      highlight-symbol-on-navigation-p t)

(global-set-key [(control shift mouse-1)]
                (lambda (event)
                  (interactive "e")
                  (goto-char (posn-point (event-start event)))
                  (highlight-symbol-at-point)))

(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

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

;; (zerodark-setup-modeline-format)

;; Tox
(setq tox-runner 'py.test)
(global-set-key "\C-ct" 'tox-current-class)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setq paradox-github-token "619e27814936e9fe86b3394f4aca33f5f7dd9b91")

(global-anzu-mode +1)
(setq anzu-cons-mode-line-p nil)

(require 'winum)
(setq winum-auto-setup-mode-line nil)
(winum-mode)

(require 'diminish)
(diminish 'anzu-mode)
(diminish 'smartparens-mode)
(diminish 'flycheck-mode)
(diminish 'highlight-symbol-mode)
;; (diminish 'projectile-mode)
(diminish 'elpy-mode)
(diminish 'org-indent-mode)
(diminish 'with-editor-mode)
(diminish 'server-buffer-clients)
(diminish 'auto-fill-function)
(diminish 'auto-revert-mode)


;; Modeline
(defun custom-modeline-modified ()
  "Show modified."
  (let* ((buffer-state (format-mode-line "%*"))
         (icon (cond
                ((string= buffer-state "-") (all-the-icons-alltheicon "arrow-right" :height 1.2 :v-adjust -0.0 :face 'success))
                ((string= buffer-state "*") (all-the-icons-alltheicon "arrow-right" :height 1.2 :v-adjust -0.0 :face 'error))
                ((string= buffer-state "%") (all-the-icons-alltheicon "arrow-right" :height 1.2 :v-adjust -0.0 :face 'custom-variable-tag)))))

    (propertize icon
                'mouse-face '(:box 0)
                'local-map (make-mode-line-mouse-map
			    'mouse-1 'read-only-mode))))


(defun -custom-modeline-github-vc ()
  "Show GIT status."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format "%s" (all-the-icons-octicon "git-branch" :face 'custom-variable-tag))
                 'display '(raise -0.1))
     (propertize (format " %s" branch) 'face 'custom-variable-tag))))

(defun -custom-modeline-svn-vc ()
  "Show SVN status."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " · %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  "Show VC status."
  (when vc-mode
    (cond
     ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
     ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
     (t (format "%s" vc-mode)))))

(defun custom-modeline-flycheck-status ()
  "Print current flycheck status."
  (let* ((text (pcase flycheck-last-status-change
		 (`finished (if flycheck-current-errors
				(let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
					       (+ (or .warning 0) (or .error 0)))))
				  (propertize (format "✖ %s Issue%s" count (unless (eq 1 count) "s")) 'face 'error))
			      (propertize "✔ No Issues" 'face 'success)))
		 (`running     (propertize "⟲ Running" 'face 'warning))
		 (`no-checker  (propertize "⚠ No Checker" 'face 'warning))
		 (`not-checked "✖ Disabled")
		 (`errored     (propertize "⚠ Error" 'face 'error))
		 (`interrupted (propertize "⛔ Interrupted" 'face 'error))
		 (`suspicious  ""))))
    (propertize text
		'help-echo "Show Flycheck Errors"
		'mouse-face '(:box 0)
		'local-map (make-mode-line-mouse-map
			    'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(setq mode-line-format '("%e" (:eval
			       (concat
				(custom-modeline-modified)
				"  "
				(propertize "%b" 'face 'bold)
				"  "
				"line %l  "
				;; (custom-modeline-window-number)
				;; (custom-modeline-mode-icon)
				(custom-modeline-icon-vc)
				"  "
				;; (custom-modeline-region-info)
				(custom-modeline-flycheck-status)
				;; (custom-modeline-suntime)
				;; (custom-modeline-weather)
				;; (custom-modeline-time)
				(mode-line-fill 'mode-line 20)
				" %m "
				))))

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
    (telephone-line base16-theme oceanic-theme ido-vertical-mode counsel-projectile counsel popwin winum spaceline diminish yaml-mode company-c-headers company-cabal company-go company-jedi solaire-mode highlight-symbol diff-hl anzu elpy git-gutter-fringe paradox rainbow-delimiters tox ini-mode window-numbering use-package zerodark-theme web-mode spaceline-all-the-icons smartparens projectile org-alert org nlinum neotree intero flycheck-color-mode-line flx-ido doom-themes dired+ company-web company-statistics company-shell company-auctex)))
 '(paradox-automatically-star t)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
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
