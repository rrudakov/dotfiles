<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#org9b4280d">1. My Emacs configuration</a>
<ul>
<li><a href="#org7d0d898">1.1. Keybindings</a>
<ul>
<li><a href="#org2d2f58d">1.1.1. Common</a></li>
<li><a href="#org925a6be">1.1.2. Python</a></li>
<li><a href="#org4c9264f">1.1.3. Org-mode</a></li>
</ul>
</li>
<li><a href="#org6ea1655">1.2. Useful commands</a>
<ul>
<li><a href="#orgc095054">1.2.1. Org-mode</a></li>
</ul>
</li>
<li><a href="#org9f9b31a">1.3. Syntax help</a>
<ul>
<li><a href="#org574c066">1.3.1. Org-mode</a></li>
</ul>
</li>
<li><a href="#orgb14f97f">1.4. Configuration</a>
<ul>
<li><a href="#orgb3dbd30">1.4.1. Common</a></li>
<li><a href="#org5af822e">1.4.2. Theme</a></li>
<li><a href="#org70f7a50">1.4.3. Semantic</a></li>
<li><a href="#org3413955">1.4.4. Autocomplete</a></li>
<li><a href="#org2d23aa4">1.4.5. Ido</a></li>
<li><a href="#org5f36a98">1.4.6. Smartparens</a></li>
<li><a href="#org0b4cbf1">1.4.7. Popwin</a></li>
<li><a href="#org4a0acbc">1.4.8. Rainbow delimiters</a></li>
<li><a href="#orge2ffa1e">1.4.9. Anzu</a></li>
<li><a href="#orgb8602b0">1.4.10. Windows navigation</a></li>
<li><a href="#org40f402a">1.4.11. Dired</a></li>
<li><a href="#orgcfa4846">1.4.12. Projectile</a></li>
<li><a href="#org23de1b3">1.4.13. Syntax check</a></li>
<li><a href="#orgc506206">1.4.14. Version control</a></li>
<li><a href="#org3323f34">1.4.15. Org</a></li>
<li><a href="#orgedd79bf">1.4.16. Languages</a></li>
<li><a href="#org635d02e">1.4.17. Set keybindings</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>

<a id="org9b4280d"></a>

# My Emacs configuration


<a id="org7d0d898"></a>

## Keybindings


<a id="org2d2f58d"></a>

### Common

    M-:	Eval command


<a id="org925a6be"></a>

### Python

    C-c C-r f Format python code
    C-c C-r r Refactoring commands


<a id="org4c9264f"></a>

### Org-mode

    C-c ^		Sort headlines
    C-c / 		Prompt to make sparce-tree
    C-c - 		Cycle list level ('-', '+', '1.', '1)' etc.)
    C-c C-x f	Footnote action command
    C-c C-c		When at footnote, go to definition, When at definition, go back to reference
    C-c |		Create new table
    M-S-<right>	Insert new table column
    C-c +		Sum all numbers in current column
    C-c C-x p	Insert property


<a id="org6ea1655"></a>

## Useful commands


<a id="orgc095054"></a>

### Org-mode

1.  Lint document

        org-lint


<a id="org9f9b31a"></a>

## Syntax help


<a id="org574c066"></a>

### Org-mode

1.  Unordered lists

        Start with '-', '+' or '*' (star not recommended)

2.  Ordered lists

        Starts with '1.' or '1)'

3.  Description list

        Unordered lists with separator '::'


<a id="orgb14f97f"></a>

## Configuration


<a id="orgb3dbd30"></a>

### Common

1.  Highlight expression in parens

        (setq show-paren-style 'expression)

2.  Highlight matching parens

        (show-paren-mode 2)

3.  Indicate empty lines at the end of the window in the left fringe

        (set-default 'indicate-empty-lines t)

4.  Enable y/n answers instead of yes/no

        (fset 'yes-or-no-p 'y-or-n-p)

5.  Highlight current line

        (global-hl-line-mode 1)

6.  Set default font

        (add-to-list 'default-frame-alist '(font . "Iosevka-10"))
        (set-frame-font "Iosevka-10")

7.  Open files ends with "rc" in conf-unix-mode

        (add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))

8.  Sen specific browser to open links

        (setq browse-url-browser-function 'browse-url-firefox)

9.  Use UTF-8 as default coding system

        (when (fboundp 'set-charset-priority)
          (set-charset-priority 'unicode))
        (prefer-coding-system        'utf-8)
        (set-terminal-coding-system  'utf-8)
        (set-keyboard-coding-system  'utf-8)
        (set-selection-coding-system 'utf-8)
        (setq locale-coding-system   'utf-8)
        (setq-default buffer-file-coding-system 'utf-8)

10. Some useful settings

        (setq-default
         ad-redefinition-action 'accept   ; silence advised function warnings
         apropos-do-all t                 ; make `apropos' more useful
         compilation-always-kill t        ; kill compilation process before starting another
         compilation-ask-about-save nil   ; save all buffers on `compile'
         compilation-scroll-output t
         confirm-nonexistent-file-or-buffer t
         enable-recursive-minibuffers nil
         ;; keep the point out of the minibuffer
         minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
         ;; History & backup settings (save nothing, that's what git is for)
         auto-save-default nil
         create-lockfiles nil
         history-length 1000
         make-backup-files nil)
        
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
         split-width-threshold 160	 ; favor horizontal splits
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

11. Be quiet at startup

        (advice-add #'display-startup-echo-area-message :override #'ignore)
        (setq inhibit-startup-message t
              inhibit-startup-echo-area-message user-login-name
              initial-major-mode 'fundamental-mode
              initial-scratch-message nil)

12. More reliable inter-window border

    The native border "consumes" a pixel of the fringe on righter-most splits,
    'window-divider' does not. Available since Emacs 25.1.
    
        (setq-default window-divider-default-places t
        	      window-divider-default-bottom-width 0
        	      window-divider-default-right-width 1)
        (window-divider-mode +1)

13. Relegate tooltips to echo area only

        (tooltip-mode -1)

14. Disable menu bar

        (menu-bar-mode -1)

15. Disable toolbar

        (when (fboundp 'tool-bar-mode)
          (tool-bar-mode -1))

16. Graphic mode settings

        (defvar my-ui-fringe-size '4 "Default fringe width.")
        
        (when (display-graphic-p)
          (scroll-bar-mode -1)
          (setq-default line-spacing 0)
          ;; buffer name  in frame title
          (setq-default frame-title-format '("RR Emacs"))
          ;; standardize fringe width
          (push (cons 'left-fringe  my-ui-fringe-size) default-frame-alist)
          (push (cons 'right-fringe my-ui-fringe-size) default-frame-alist))

17. Use SSH for tramp-mode

        (setq tramp-default-method "ssh")

18. Set default input method

        (setq default-input-method "russian-computer")


<a id="org5af822e"></a>

### Theme

1.  All the icons

        (use-package all-the-icons :ensure t)

2.  Color scheme

        (use-package gruvbox-theme
          :ensure t
          :config
          (load-theme 'gruvbox-dark-hard t))

3.  Highlight symbol at point

        (use-package highlight-symbol
          :ensure t
          :config
          (highlight-symbol-nav-mode)
        
          (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
        
          (setq highlight-symbol-idle-delay 0.2
        	highlight-symbol-on-navigation-p t)
        
          (global-set-key [(control shift mouse-1)]
        		  (lambda (event)
        		    (interactive "e")
        		    (goto-char (posn-point (event-start event)))
        		    (highlight-symbol-at-point)))
        
          (global-set-key (kbd "M-n") 'highlight-symbol-next)
          (global-set-key (kbd "M-p") 'highlight-symbol-prev))

4.  Modeline

    1.  Custom functions
    
        1.  Git status
        
                (defun rr/modeline-git-vc ()
                  "Show GIT status."
                  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
                    (when (buffer-file-name)
                      (pcase (vc-state (buffer-file-name))
                	(`up-to-date
                	 (concat
                	  (propertize (all-the-icons-octicon "git-branch")
                		      'face `(:inherit mode-line :family ,(all-the-icons-octicon-family) :height 1.2)
                		      'display '(raise -0.1))
                	  (propertize (format " %s" branch) 'face `(:inherit mode-line))))
                	(`edited
                	 (concat
                	  (propertize (all-the-icons-octicon "git-branch")
                		      'face `(:inherit mode-line :foreground "#87afaf" :family ,(all-the-icons-octicon-family) :height 1.2)
                		      'display '(raise -0.1))
                	  (propertize (format " %s" branch) 'face `(:inherit mode-line :foreground "#87afaf"))))
                	(`added
                	 (concat
                	  (propertize (all-the-icons-octicon "git-branch")
                		      'face `(:inherit mode-line :foreground "#b8bb26" :family ,(all-the-icons-octicon-family) :height 1.2)
                		      'display '(raise -0.1))
                	  (propertize (format " %s" branch) 'face `(:inherit mode-line :foreground "#b8bb26"))))
                	(`unregistered " ??")
                	(`removed
                	 (concat
                	  (propertize (all-the-icons-octicon "git-branch")
                		      'face `(:inherit mode-line :foreground "#fb4934" :family ,(all-the-icons-octicon-family) :height 1.2)
                		      'display '(raise -0.1))
                	  (propertize (format " %s" branch) 'face `(:inherit mode-line :foreground "#fb4934"))))
                	(`needs-merge " Con")
                	(`needs-update " Upd")
                	(`ignored " Ign")
                	(_ " Unk")))))
        
        2.  SVN status
        
                (defun rr/modeline-svn-vc ()
                  "Show SVN status."
                  (let ((revision (cadr (split-string vc-mode "-"))))
                    (concat
                     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
                     (propertize (format " · %s" revision) 'face `(:height 0.9)))))
        
        3.  Flycheck status
        
                (defun rr/flycheck-status-text ()
                  (let* ((text (pcase flycheck-last-status-change
                		 (`finished (if flycheck-current-errors
                				(let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                					       (+ (or .warning 0) (or .error 0)))))
                				  (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s")) 'face `(:inherit mode-line :foreground "#fb4934")))
                			      (propertize "✔ No Issues" 'face `(:inherit mode-line :foreground "#afaf00"))))
                		 (`running     (propertize "⟲ Running" 'face `(:inherit mode-line :foreground "#87afaf")))
                		 (`no-checker  (propertize "⚠ No Checker" 'face `(:inherit mode-line)))
                		 (`not-checked (propertize "✖ Disabled" 'face' `(:inherit mode-line)))
                		 (`errored     (propertize "⚠ Error" 'face `(:inherit mode-line :foreground "#fb4934")))
                		 (`interrupted (propertize "⛔ Interrupted" 'face `(:inherit mode-line :foreground "#fb4934")))
                		 (`suspicious  ""))))
                    (propertize text)))
        
        4.  Update vc-state in all buffers
        
                (defun rr/refresh-vc-state ()
                  "Refresh vc-state on all buffers."
                  (dolist (buff (buffer-list))
                    (with-current-buffer buff
                      (when (vc-mode)
                	(vc-refresh-state)))))
    
    2.  Spaceline
    
        1.  Faces
        
                (defface rr/spaceline-unmodified
                  '((t :inherit 'spaceline-unmodified :background "#87afaf"))
                  "Face for unmodified buffer in the mode-line.")
                
                (defface rr/spaceline-modified
                  '((t :inherit 'spaceline-modified :background "#d75f5f"))
                  "Face for modified buffer in the mode-line.")
                
                (defface rr/spaceline-read-only
                  '((t :inherit 'spaceline-read-only :background "#d787af"))
                  "Face for read-only buffer in the mode-line.")
                
                (defun rr/spaceline-face (face active)
                  "For spaceline-face-func get FACE and ACTIVE."
                  (pcase (cons face active)
                    ('(face1 . t)   'mode-line)
                    ('(face1 . nil) 'mode-line-inactive)
                    ('(face2 . t)   'mode-line)
                    ('(face2 . nil) 'mode-line-inactive)
                    ('(line . t)    'mode-line)
                    ('(line . nil)  'mode-line-inactive)
                    ('(highlight . t)
                     (cond
                      (buffer-read-only 'rr/spaceline-read-only)
                      ((buffer-modified-p) 'rr/spaceline-modified)
                      (t 'rr/spaceline-unmodified)))
                    ('(highlight . nil) 'powerline-inactive1)
                    (_ 'error)))
        
        2.  Common configuration
        
                (use-package spaceline :ensure t
                  :config
                  (setq powerline-height 30)
                  (setq powerline-default-separator 'utf-8)
                  (setq spaceline-separator-dir-left '(right . right))
                  (setq spaceline-separator-dir-right '(right . right))
                  (setq powerline-default-separator 'alternate)
                  (setq spaceline-window-numbers-unicode t)
                  (setq spaceline-face-func 'rr/spaceline-face))
        
        3.  Custom theme
        
                (require 'spaceline-config)
                
                ;; Define custom segments
                (spaceline-define-segment rr/version-control
                  "Show VC status."
                  (when vc-mode
                    (cond
                     ((string-match "Git[:-]" vc-mode) (rr/modeline-git-vc))
                     ((string-match "SVN-" vc-mode) (rr/modeline-svn-vc))
                     (t (propertize (format "%s" vc-mode)))))
                  :enabled t)
                
                (spaceline-define-segment rr/flycheck-status
                  "Print current flycheck status."
                  (when (and (bound-and-true-p flycheck-mode))
                    (format "%s " (rr/flycheck-status-text)))
                  :enabled t)
                
                ;; My custom theme
                (defun rr/spaceline-theme (&rest additional-segments)
                  "Apply my spaceline theme ADDITIONAL-SEGMENTS are inserted on the right."
                  (spaceline-install
                    `(((workspace-number
                	window-number)
                       :fallback evil-state
                       :face highlight-face
                       :priority 0)
                      (anzu :priority 4)
                      auto-compile
                      ((buffer-id remote-host)
                       :priority 5)
                      (point-position line-column)
                      (buffer-position :priority 0)
                      (process :when active)
                      (mu4e-alert-segment :when active)
                      (erc-track :when active)
                      (org-pomodoro :when active)
                      (org-clock :when active))
                    `(which-function
                      (python-pyvenv :fallback python-pyenv)
                      purpose
                      (battery :when active)
                      (selection-info :priority 2)
                      input-method
                      (buffer-encoding-abbrev :priority 3)
                      (global :when active)
                      ,@additional-segments
                      (rr/version-control :when active :priority 7)
                      major-mode
                      (rr/flycheck-status :when active)))
                
                  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
                
                (rr/spaceline-theme)
                (spaceline-compile)


<a id="org70f7a50"></a>

### Semantic

1.  Enable semantic mode global

        (semantic-mode 1)

2.  Highlight current function

        (global-semantic-highlight-func-mode 1)

3.  Show function definition at the top of buffer

        (global-semantic-stickyfunc-mode 1)

4.  Enable database

        (global-semanticdb-minor-mode 1)


<a id="org3413955"></a>

### Autocomplete

1.  Common

        (use-package company
          :ensure t
          :init
          (setq company-dabbrev-downcase nil)
          :config
          (setq company-auto-complete nil)
          (add-hook 'after-init-hook 'global-company-mode))

2.  Statistic

    Show more offten used completeons first
    
        (use-package company-statistics
          :ensure t
          :config
          (company-statistics-mode))

3.  Quick help

        (use-package company-quickhelp
          :ensure t
          :config
          (company-quickhelp-mode 1))

4.  Languages

    1.  LaTeX
    
            (use-package company-auctex
              :ensure t
              :config
              (company-auctex-init))
    
    2.  WEB
    
            (use-package company-web
              :ensure t
              :config
              (add-to-list 'company-backends 'company-web-html)
              (add-to-list 'company-backends 'company-web-jade)
              (add-to-list 'company-backends 'company-web-slim))
    
    3.  Shell
    
            (use-package company-shell
              :ensure t
              :config
              (add-to-list 'company-backends 'company-shell))

5.  Solve company and yasnippet conflict

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


<a id="org2d23aa4"></a>

### Ido

1.  Enable ido global

        (ido-mode 1)
        (ido-everywhere 1)

2.  Set keybindings

    Buffer select
    
        (global-set-key (kbd "<f2>") 'bs-show)
    
    Redefine M-x with ido-mode
    
        (global-set-key
         "\M-x"
         (lambda ()
           (interactive)
           (call-interactively
            (intern
             (ido-completing-read
              "M-x "
              (all-completions "" obarray 'commandp))))))

3.  Enable Flex ido

        (use-package flx-ido
          :ensure t
          :config
          (flx-ido-mode 1)
          (setq ido-enable-flex-matching t)
          (setq ido-use-faces nil))

4.  Vertical ido

        (use-package ido-vertical-mode
          :ensure t
          :config
          (ido-vertical-mode 1)
          (setq ido-vertical-define-keys 'C-n-and-C-p-only)
          (setq ido-vertical-show-count t))


<a id="org5f36a98"></a>

### Smartparens

    (use-package smartparens
      :ensure t
      :init
      (setq sp-show-pair-delay 0.1
    	sp-show-pair-from-inside t)
      :config
      (require 'smartparens-config)
      (smartparens-global-mode)
      (show-smartparens-global-mode)
      (setq smartparens-strict-mode t))


<a id="org0b4cbf1"></a>

### Popwin

    (use-package popwin
      :ensure t
      :config
      (popwin-mode 1))


<a id="org4a0acbc"></a>

### Rainbow delimiters

    (use-package rainbow-delimiters
      :ensure t
      :config
      (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


<a id="orge2ffa1e"></a>

### Anzu

    (use-package anzu
      :ensure t
      :config
      (global-anzu-mode +1)
      (setq anzu-cons-mode-line-p nil))


<a id="orgb8602b0"></a>

### Windows navigation

    (use-package winum
      :ensure t
      :config
      (setq winum-auto-setup-mode-line nil)
      (winum-mode))


<a id="org40f402a"></a>

### Dired

    (use-package dired+
      :ensure t
      :config
      (setq dired-listing-switches "--group-directories-first -alh")
      (toggle-diredp-find-file-reuse-dir 1))


<a id="orgcfa4846"></a>

### Projectile

    (use-package projectile
      :ensure t
      :config
      (projectile-global-mode)
      (add-to-list 'projectile-globally-ignored-files "*.log")
      (setq projectile-enable-caching t)
      (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name)))))


<a id="org23de1b3"></a>

### Syntax check

1.  Flycheck

        (use-package flycheck
          :ensure t
          :config
          (add-hook 'after-init-hook #'global-flycheck-mode)
        
          (setq flycheck-indication-mode 'right-fringe)
        
          (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
            [0 0 0 0 0 4 12 28 60 124 252 124 60 28 12 4 0 0 0 0]))


<a id="orgc506206"></a>

### Version control

1.  GIT

        (use-package magit
          :ensure t
          :config
          (global-set-key (kbd "C-x g") 'magit-status)
          (add-hook 'focus-in-hook 'rr/refresh-vc-state)
          (add-hook 'magit-post-refresh-hook 'rr/refresh-vc-state))

2.  Highlight diff

        (use-package diff-hl
          :ensure t
          :init
          (setq-default fringes-outside-margins t)
        
          :config
          (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
          (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
          (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


<a id="org3323f34"></a>

### Org

1.  Some tweaks

        (add-hook 'org-mode-hook 'turn-on-font-lock)
        (add-hook 'org-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
        (setq org-startup-folded 'content) ;; overview | content | all | showeverything
        (setq org-cycle-separator-lines 1)
        (setq org-tags-column -100)
        (setq org-startup-indented t)
        (setq org-hide-leading-stars t)
        (setq org-adapt-indentation nil)
        (setq org-src-window-setup 'current-window)
        (setq org-catch-invisible-edits 'show-and-error)
        (setq org-return-follows-link t)
        (add-to-list 'org-modules 'org-habit)

2.  Agenda settings

        (setq org-agenda-files (quote ("~/Org")))
        (setq org-directory "~/Org")
        (setq org-enforce-todo-dependencies t)
        (setq org-agenda-restore-windows-after-quit t)

3.  Capture settings

        (setq org-default-notes-file (concat org-directory "/notes.org"))
        
        (setq org-capture-templates
              '(("t" "Todo" entry (file+headline "" "Tasks")
        	 "* TODO %?\n%i"
        	 :clock-in t
        	 :clock-resume t)))
        
        (setq org-refile-targets (quote ((nil :maxlevel . 9)
        				 (org-agenda-files :maxlevel . 9))))

4.  Tag list

        (setq org-tag-alist '((:startgroup . nil)
        		      ("@critical" . ?C)
        		      ("@medium" . ?M)
        		      ("@low" . ?L)
        		      (:endgroup . nil)))

5.  Keywords list

        (setq org-todo-keywords
              '((sequence "TODO(t)" "PROGRESS(p!)" "PAUSED(h!)" "|" "DONE(d!)")))

6.  Custom keywords faces

        (setq org-todo-keyword-faces
              '(("PROGRESS" :foreground "#b57614" :weight bold)
        	("PAUSED" :foreground "#5f8787" :weight bold)))

7.  Clock settings

        (setq org-clock-persist 'history)
        (org-clock-persistence-insinuate)
        (setq org-log-into-drawer t)
        (setq org-log-done 'time)
        
        (defun rr/set-progress (last)
          "Set PROGRESS state if LAST is different."
          (when (not (string-equal last "PROGRESS"))
            (let ()
              (remove-hook 'org-after-todo-state-change-hook 'rr/start-clock)
              "PROGRESS")))
        
        (add-hook 'org-clock-in-hook
        	  (lambda ()
        	    (add-hook 'org-after-todo-state-change-hook 'rr/start-clock)))
        
        (setq org-clock-in-switch-to-state 'rr/set-progress)
        
        (setq non-clocking-states '("PAUSED" "DONE"))
        
        (defun rr/ido-non-clocking-state ()
          "Prompt to select non-clocking state."
          (interactive)
          (message "%s" (ido-completing-read "Select state: " non-clocking-states)))
        
        (defun rr/after-clock-stop (last)
          "Change TASK state after clock stop depends on LAST state."
          (when (not (or (string-equal last "PAUSED")
        		 (string-equal last "DONE")
        		 (string-equal last "TODO")))
            (let ()
              (remove-hook 'org-after-todo-state-change-hook 'rr/stop-clock)
              (if (y-or-n-p "Current task DONE? ")
        	    "DONE"
        	  "PAUSED"))))
        
        (add-hook 'org-clock-out-hook
        	  (lambda ()
        	    (add-hook 'org-after-todo-state-change-hook 'rr/stop-clock)))
        
        (setq org-clock-out-switch-to-state 'rr/after-clock-stop)
        
        (defun rr/stop-clock ()
          "Stop clock if task state changed to PAUSED or DONE."
          (when (and (org-clocking-p)
        	     (or (string-equal org-state "PAUSED")
        		 (string-equal org-state "DONE")
        		 (string-equal org-state "TODO"))
        	     (< (point) org-clock-marker)
        	     (> (save-excursion (outline-next-heading) (point))
        		org-clock-marker))
            (let ((org-log-note-clock-out nil)
        	  (org-clock-out-switch-to-state nil))
              (org-clock-out))))
        
        (add-hook 'org-after-todo-state-change-hook 'rr/stop-clock)
        
        (defun rr/start-clock ()
          "Start clock if task state changed to PROGRESS."
          (if (org-clocking-p)
              (when (and (string-equal org-state "PROGRESS")
        		 (not (string-equal (nth 4 (org-heading-components)) org-clock-heading)))
        	(let ((org-clock-in-switch-to-state nil))
        	  (org-clock-in)))
            (when (string-equal org-state "PROGRESS")
              (let ((org-clock-in-switch-to-state nil))
        	(org-clock-in)))))
        
        (add-hook 'org-after-todo-state-change-hook 'rr/start-clock)

8.  Appearance

    1.  Org bullets
    
            (use-package org-bullets
              :ensure t
              :init
            
              (setq org-bullets-bullet-list '("•"))
            
              (setq org-ellipsis "…")
            
              :config
              (add-hook 'org-mode-hook #'org-bullets-mode))
    
    2.  Org source code
    
            (setq org-src-fontify-natively t)
            
            (require 'color)
            (set-face-attribute 'org-block nil :background
            		    (color-darken-name
            		     (face-attribute 'default :background) 3))
            
            (setq org-src-block-faces '(("emacs-lisp" (:background "#282828"))
            			    ("python" (:background "#282828"))))

9.  Alerts

        (use-package org-alert
          :ensure t
          :config
          (setq org-alert-enable t)
          (setq alert-default-style 'libnotify))

10. Markdown export

        (require 'ox-md nil t)


<a id="orgedd79bf"></a>

### Languages

1.  Haskell

    1.  Intero
    
            (use-package intero
              :ensure t
              :config
              (add-hook 'haskell-mode-hook 'intero-mode))
    
    2.  Align rules
    
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
    
    3.  Haskell doc mode
    
            (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

2.  LaTeX

    1.  AucREX tweaks
    
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

3.  Python

    1.  Virtualenv
    
            (use-package virtualenvwrapper
              :ensure t
              :config
              (venv-initialize-interactive-shells)
              (venv-initialize-eshell))
    
    2.  Elpy
    
            (use-package elpy
              :ensure t
              :config
              (elpy-enable)
              (setq elpy-rpc-backend "jedi")
              (add-hook 'elpy-mode-hook 'yas-minor-mode-on))
    
    3.  Tox
    
            (use-package tox
              :ensure t
              :config
              (setq tox-runner 'py.test)
              (global-set-key "\C-ct" 'tox-current-class))

4.  JSON

        (use-package json-mode
          :ensure t
          :config
          (setq json-reformat:pretty-string? t))

5.  YAML

        (use-package yaml-mode
          :ensure t
          :config
          (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

6.  WEB

        (use-package web-mode
          :after smartparens
          :ensure t
          :config
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
        
          (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))


<a id="org635d02e"></a>

### Set keybindings

1.  Common

        (global-set-key (kbd "M-[") 'align)

2.  Org

        (global-set-key "\C-cl" 'org-store-link)
        (global-set-key "\C-ca" 'org-agenda)
        (global-set-key "\C-cc" 'org-capture)
        (global-set-key "\C-cb" 'org-iswitchb)
        
        (global-set-key (kbd "<f5>") 'org-clock-goto)
        (global-set-key (kbd "<f6>") 'org-clock-in)
        (global-set-key (kbd "<f7>") 'org-clock-out)
        ;; (global-set-key (kbd "<f7>") (lambda ()
        ;;                                (interactive)
        ;;                                (org-call-with-arg 'org-todo "PAUSED")))
        ;; (global-set-key (kbd "<f8>") (lambda ()
        ;;                                (interactive)
        ;;                                (org-call-with-arg 'org-todo "DONE")))
        (global-set-key "\C-cm" 'org-agenda-month-view)

