(setq user-full-name "Prateek Sharma"
      user-mail-address "sharmajiprateek9@gmail.com")

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq warning-minimum-level :emergency)

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))))

(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))
(set-face-attribute 'default nil :font "JetBrains Mono-11")
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono-11")
(set-face-attribute 'variable-pitch nil :font "JetBrains Mono-11")

(setq-default
 visual-bell t
 read-process-output-max (* 3 1024 1024)
 indent-tabs-mode nil
 set-mark-command-repeat-pop t
 vc-follow-symlinks t)

(setq confirm-kill-emacs 'y-or-n-p)

(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t
      use-dialog-box nil
      make-pointer-invisible t
      load-prefer-newer t
      max-lisp-eval-depth 10000
      max-specpdl-size 10000
      scroll-margin 8
      auto-revert-check-vc-info t
      scroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      save-interprogram-paste-before-kill t
      isearch-lazy-count t
      indicate-buffer-boundaries t
      indicate-empty-lines t)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(defun scroll-up-half ()
  (interactive)
  (scroll-up-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))

(defun scroll-down-half ()
  (interactive)
  (scroll-down-command
   (floor
    (- (window-height)
       next-screen-context-lines)
    2)))

(global-set-key [remap scroll-up-command] 'scroll-up-half)
(global-set-key [remap scroll-down-command] 'scroll-down-half)
(global-set-key [remap zap-to-char] 'zap-up-to-char)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

(setq user-emacs-directory (expand-file-name "~/.local/share/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(pixel-scroll-precision-mode t)
(mouse-avoidance-mode 'cat-and-mouse)
(electric-pair-mode 1)
(context-menu-mode t)
(blink-cursor-mode -1)
(winner-mode 1)
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(save-place-mode t)
(recentf-mode t)
(global-display-line-numbers-mode)
;; (global-hl-line-mode)
(global-auto-revert-mode 1)
(make-variable-buffer-local 'global-hl-line-mode)
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0)
                   (global-hl-line-mode 0))))

(setq frame-title-format
      '(""
        (:eval
         (let ((project-name (nth 2 (project-current))))
           (unless (not project-name)
             (format "[%s] " project-name))))
        "%b"))

(use-package general)

(use-package all-the-icons)

(use-package org
  :mode ("\\.org$" . org-mode)
  :config
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package harpoon
  :straight t
  :custom
  (harpoon-project-package 'project)
  :init
  (define-prefix-command 'harpoon-map)
  (global-set-key (kbd "C-'") 'harpoon-map)
  :bind (:map harpoon-map
              ("h" . harpoon-toggle-file)
              ("'" . harpoon-add-file)
              ("c" . harpoon-clear)
              ("r" . harpoon-toggle-quick-menu)
              ("1" . harpoon-go-to-1)
              ("2" . harpoon-go-to-2)
              ("3" . harpoon-go-to-3)
              ("4" . harpoon-go-to-4)
              ("8" . harpoon-go-to-5)
              ("9" . harpoon-go-to-6)
              ("0" . harpoon-go-to-7))
  :config
  (setq harpoon-cache-file (concat user-emacs-directory "harpoon/")))

(use-package project
  :straight (:type built-in)
  :bind (:map project-prefix-map
              ("t" . project-todo))
  :config
  (defun project-todo ()
  "Edit the TODO.org file at the root of the current project."
  (interactive)
  (let* ((base (ignore-errors (project-root (project-current))))
         (todo (file-name-concat base "TODO.org")))
    (cond ((and base (file-exists-p todo)) (find-file todo))
          ((not base) (error "Not in a project"))
          (t (error "Project does not contain a TODO.org file.")))))
  (add-to-list 'project-switch-commands '(project-todo "Todo" "t")))

(use-package ibuffer
  :config
  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

(use-package ibuffer-project
  :after (ibuffer project)
  :config
  (add-hook
   'ibuffer-hook
   (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative))))

  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative))))

  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " project-file-relative))))

(general-def ctl-x-map
  "C-b" nil
  "B" 'ibuffer-jump)

(use-package hl-todo
  :straight t
  :defer 1
  :config
  (setq hl-todo-keyword-faces '(("TODO" . "#FF0000")
                                ("FIXME" . "#FF0000")
                                ("GOTCHA" . "#FF4500")
                                ("STUB" . "#1E90FF")
                                ("NOTE" . "#0090FF")
                                ("XXX" . "#AF0494")))
  (global-hl-todo-mode))

(use-package avy
  :custom
  (avy-timeout-seconds 0.4)
  :bind
  ("M-j" . avy-goto-char-timer)
  ("M-n" . avy-goto-line-below)
  ("M-p" . avy-goto-line-above))

(use-package display-fill-column-indicator
  :straight (:type built-in)
  :hook
  (python-ts-mode . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column 100)
  ;; (setq display-fill-column-indicator-character "|")
  )

(use-package files
  :straight (:type built-in)
  :custom
  (backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (backup-by-copying t)               ; Always use copying to create backup files
  (delete-old-versions t)             ; Delete excess backup versions
  (kept-new-versions 6)               ; Number of newest versions to keep when a new backup is made
  (kept-old-versions 2)               ; Number of oldest versions to keep when a new backup is made
  (version-control t)                 ; Make numeric backup versions unconditionally
  (auto-save-default nil)             ; Stop creating #autosave# files
  (mode-require-final-newline nil)    ; Don't add newlines at the end of files
  (large-file-warning-threshold nil)) ; Open large files without requesting confirmation

(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
        create-lockfiles nil ; don't create .# files (crashes 'npm start')
        make-backup-files nil))

(use-package no-littering)
(use-package diminish)
(use-package delight)
(use-package popup)
(use-package gcmh
  :diminish
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 100 1024 1024))
  (gcmh-mode 1))
(use-package bug-hunter)
(use-package restart-emacs)
(use-package free-keys)

(use-package multiple-cursors
  :custom
  (mc/always-run-for-all t)
  :bind
  (("C-*" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-M->" . mc/skip-to-next-like-this)
   ("C-M-<" . mc/skip-to-previous-like-this)
   ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package dirvish
  :straight (dirvish :files (:defaults "extensions/*"))
  :config
  (dirvish-override-dired-mode))

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history nil)

  (fset 'undo-auto-amalgamate 'ignore)
  (setq undo-limit 6710886400)
  (setq undo-strong-limit 100663296)
  (setq undo-outer-limit 1006632960))

(use-package hydra)

(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-buffer
                                vertico-multiform
                                vertico-directory
                                vertico-flat
                                vertico-indexed
                                vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse))
  :config
  (setq vertico-count 12)
  (setq vertico-cycle t)
  (define-key vertico-map (kbd "<S-backspace>") 'vertico-directory-up)

  (define-key vertico-map (kbd "M-n") 'vertico-next-group)
  (define-key vertico-map (kbd "M-p") 'vertico-previous-group)

  ;; Do not allow the cursor in the minibuffer prompt
  ;; (setq minibuffer-prompt-properties
  ;;       '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer indexed)
          (consult-xref buffer indexed)
          (consult-imenu buffer)
          (consult-buffer)
          (xref-find-references buffer)))
  (setq vertico-multiform-categories
        '((consult-grep buffer))))

(use-package vertico-mouse
  :after vertico
  :config
  (vertico-mouse-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


(use-package vertico-directory
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (orderless-component-separator 'orderless-escapable-split-on-space)
  ;; (completion-category-defaults nil)
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(
                               orderless-literal
                               orderless-regexp
                               orderless-prefixes
                               orderless-initialism
                               orderless-flex
                               ))
  :config
  (defun orderless-company-completion (fn &rest args)
    "Highlight company matches correctly, and try default completion styles before
orderless."
    (let ((orderless-match-faces [completions-common-part])
          (completion-styles '(basic partial-completion orderless)))
      (apply fn args)))
  (advice-add 'company-capf--candidates :around 'orderless-company-completion)

  (orderless-define-completion-style orderless+basic
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp)))
  (setq completion-category-overrides
        '((command (styles orderless+basic))
          (symbol (styles orderless+basic))
          (variable (styles orderless+basic))
          (file (styles basic partial-completion)))))

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> t" . consult-theme)             ;; orig. help-with-tutorial
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :after consult)

(use-package consult-git-log-grep
  :straight t
  :after consult
  :commands (consult-git-log-grep)
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package embark
  :bind
  (("C-," . embark-act)
   ("C-M-," . embark-dwim)
   ("<f1> B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package zenburn-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package afternoon-theme)
(use-package flatland-theme)
(use-package solarized-theme)
(use-package zeno-theme)
(use-package dracula-theme)
(use-package ef-themes)
(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config))
(use-package nordic-night-theme
  :straight (:type git :repo "https://git.sr.ht/~ashton314/nordic-night" :branch "main"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package elune-theme)

;; Load Themes
;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))

(use-package emacs
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-subtle-line-numbers t
        modus-themes-region '(bg-only no-extend)
        modus-themes-mode-line '(accented borderless)
        ;; modus-themes-hl-line '(accented)
        modus-themes-parens-match '(bold intense))

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)


  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  :config
  ;; Load the theme of your choice:
  (load-theme 'ef-dark t))

(if (not (version<= emacs-version "29.0"))
    (use-package treesit-auto
      :demand t
      :config
      (setq treesit-auto-install 'prompt)
      (setq my-js-tsauto-config
        (make-treesit-auto-recipe
         :lang 'javascript
         :ts-mode 'js-ts-mode
         :remap '(js2-mode js-mode javascript-mode)
         :url "https://github.com/tree-sitter/tree-sitter-javascript"
         :revision "master"
         :source-dir "src"))
      (add-to-list 'treesit-auto-recipe-list my-js-tsauto-config)
      (global-treesit-auto-mode)))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package highlight-indent-guides
  :disabled
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'stack))

(use-package xref
  :straight (:type built-in)
  :config
  (setq xref-history-storage 'xref-window-local-history))

(use-package flycheck)
(use-package consult-flycheck)

(use-package flymake
  :disabled t
  :straight nil
  :config
  (defhydra flymake-map (flymake-mode-map "C-c f")
    "flymake"
    ("n" flymake-goto-next-error "next-error")
    ("p" flymake-goto-prev-error "prev-error")
    ("f" flymake-show-buffer-diagnostics "buffer diagnostics"))
  :hook (prog-mode . flymake-mode))

(use-package flymake-diagnostic-at-point
  :disabled t
  :hook flymake-mode
  :custom
  (flymake-diagnostic-at-point-timer-delay 0.8))

(use-package eglot
  :disabled t
  :straight nil
  :bind (("C-c l e" . eglot)
         :map eglot-mode-map
         ("C-c l r" . eglot-rename)
         ("C-c l a" . eglot-code-actions)
         ("C-c l f" . eglot-format))
  :custom
  (eglot-autoshutdown t)
  :init
  (which-key-add-key-based-replacements "C-c l" "eglot")
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio"))))
  ;; (setcdr (assq 'java-mode eglot-server-programs)
  ;;         `("jdtls" "-data" "/home/pr09eek/.cache/emacs/workspace/"
  ;;        "-Declipse.application=org.eclipse.jdt.ls.core.id1"
  ;;    "-Dosgi.bundles.defaultStartLevel=4"
  ;;    "-Declipse.product=org.eclipse.jdt.ls.core.product"
  ;;    "-Dlog.level=ALL"
  ;;    "-noverify"
  ;;    "-Xmx1G"
  ;;    "--add-modules=ALL-SYSTEM"
  ;;    "--add-opens java.base/java.util=ALL-UNNAMED"
  ;;    "--add-opens java.base/java.lang=ALL-UNNAMED"
  ;;    "-jar ./plugins/org.eclipse.equinox.launcher_1.5.200.v20180922-1751.jar"
  ;;    "-configuration ./config_linux")))

(use-package consult-eglot
  :diabled t
  :after (eglot consult))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  (lsp-file-watch-threshold 100000)
  (lsp-keymap-prefix "C-c l")
  :init
  (setq lsp-idle-delay 0
        lsp-signature-doc-lines 2)
  ;; (defun my/lsp-mode-setup-completion ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(orderless))) ;; Configure orderless
  ;; :hook
  ;; (lsp-completion-mode . my/lsp-mode-setup-completion)
  :commands (lsp lsp-deferred)
  :config
  (dolist (mode '(c-ts-mode-hook
                  js-ts-mode-hook
                  typescript-ts-mode-hook
                  c++-ts-mode-hook))
    (add-hook mode 'lsp-deferred))
  ;; Add buffer local Flycheck checkers after LSP for different major modes.
  ;; (defvar-local my-flycheck-local-cache nil)
  ;; (defun my-flycheck-local-checker-get (fn checker property)
  ;;   ;; Only check the buffer local cache for the LSP checker, otherwise we get
  ;;   ;; infinite loops.
  ;;   (if (eq checker 'lsp)
  ;;       (or (alist-get property my-flycheck-local-cache)
  ;;           (funcall fn checker property))
  ;;     (funcall fn checker property)))
  ;; (advice-add 'flycheck-checker-get
  ;;             :around 'my-flycheck-local-checker-get)

  ;; (add-hook 'lsp-managed-mode-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'python-ts-mode)
  ;;               (setq my-flycheck-local-cache '((next-checkers . (python-pylint)))))))

  (use-package consult-lsp)
  (general-def lsp-mode-map
    [remap xref-find-apropos] 'consult-lsp-symbols)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable nil)
  (lsp-headerline-breadcrumb-enable nil))

(use-package dap-mode)
(use-package lsp-treemacs)

(use-package lsp-java
  :hook (java-ts-mode . lsp-deferred))

(use-package lsp-pyright
  :init
  (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  (setq lsp-pyright-multi-root nil
        lsp-pyright-typechecking-mode "off")
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))  ; or lsp

(use-package pyvenv
  :hook ((python-ts-mode . pyvenv-mode)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :init
  (setq company-text-icons-add-background t)
  :custom
  (company-minimum-prefix-length 1)
  (company-abort-on-unique-match nil)
  (company-show-quick-access t)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-dabbrev-other-buffers nil)
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.0)
  (compan-tooltip-idle-delay 0.1)
  (company-backends '(company-capf company-yasnippet company-files))
  (company-text-icons-add-background t)
  (company-format-margin-function #'company-text-icons-margin)
  (company-frontends '(company-pseudo-tooltip-frontend))
  (company-tooltip-minimum 8)
  :config
  (global-company-mode))

(use-package company-quickhelp
  :disabled t
  :hook company-mode)

;; (use-package company-posframe
;;   :hook company-mode)

(use-package company-box
  :disabled
  :straight (company-box :type git
                         :host github
                         :repo "sebastiencs/company-box"
                         :branch "master")
  :hook (company-mode . company-box-mode))

;; (use-package corfu
;;   :straight (corfu :files (:defaults "extensions/*"))
;;   :bind (:map corfu-map
;;               ("M-j" . corfu-quick-complete))
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-auto-prefix 1)
;;   (corfu-auto-delay 0.1)
;;   (corfu-separator ?\s)          ;; Orderless field separator
;;   (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
;;   (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
;;   (corfu-preview-current t)    ;; Disable current candidate preview
;;   (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode))

;; (use-package corfu-doc
;;   :after corfu
;;   :bind (:map corfu-map
;;               ("M-d" . corfu-doc-toggle)
;;               ("M-n" . corfu-doc-scroll-up)
;;               ("M-p" . corfu-doc-scroll-down)))

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ;; Add extensions
;; (use-package cape
;;   :hook corfu
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-history)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-tex)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   (add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;;   ;;(add-to-list 'completion-at-point-functions #'cape-line)
;;   )

(use-package docker-tramp)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :disabled t
  :hook (prog-mode . copilot-mode)
  :config
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

  (defun rk/copilot-quit ()
    "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
    (interactive)
    (condition-case err
        (when copilot--overlay
          (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
                       (setq copilot-disable-predicates (list (lambda () t)))
                       (copilot-clear-overlay)
                       (run-with-idle-timer
                        1.0
                        nil
                        (lambda ()
                          (setq copilot-disable-predicates pre-copilot-disable-predicates)))))))
  (advice-add 'keyboard-quit :before #'rk/copilot-quit))

(use-package magit
  :config
  (magit-auto-revert-mode t))

(use-package git-gutter-fringe
  :diminish
  :config
  (setq git-gutter:update-interval 0.1)
  (global-git-gutter-mode)
  (diminish 'git-gutter-mode " GG")
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package vterm
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000))

(use-package restclient
  :straight t
  :defer t
  :mode ("\\.rest\\'". restclient-mode)
  :config (add-hook 'restclient-mode-hook (lambda ()
                                            (setq imenu-generic-expression '((nil "^#+\s+.+" 0))))))

(use-package proced
  :config
  (setq proced-enable-color-flag t))

(use-package redacted
  :config
  (add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1)))))

(use-package lemon
  :disabled t
  :straight (:type git :repo "https://codeberg.org/emacs-weirdware/lemon.git")
  :config (lemon-mode 1))

(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defadvice backward-kill-word (around delete-pair activate)
  (if (eq (char-syntax (char-before)) ?\()
      (progn
        (backward-char 1)
        (save-excursion
          (forward-sexp 1)
          (delete-char -1))
        (forward-char 1)
        (append-next-kill)
        (kill-backward-chars 1))
    ad-do-it))

;; need to improve this
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
  Ease of use features:
    - Move to start of next line.
    - Appends the copy on sequential calls.
    - Use newline as last char even on the last line of the buffer.
    - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun ps/indent-buffer ()
  "Indent the current buffer"
  (interactive)
  (indent-region (point-min) (point-max)))
