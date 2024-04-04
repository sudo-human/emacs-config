(setq user-full-name "Prateek Sharma"
      user-mail-address "sharmajiprateek9@gmail.com")

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))))

(add-to-list 'default-frame-alist '(font . "Iosevka Comfy-15"))
(set-face-attribute 'default nil :font "Iosevka Comfy-15")
(set-face-attribute 'fixed-pitch nil :font "Iosevka Comfy-15")
(set-face-attribute 'variable-pitch nil :font "Iosevka Comfy-15")

(setq-default visual-bell t
              read-process-output-max (* 3 1024 1024)
              indent-tabs-mode nil
              set-mark-command-repeat-pop t
              vc-follow-symlinks t
              grep-use-headings t
              indicate-buffer-boundaries t
              scroll-preserve-screen-position 1
              indicate-empty-lines t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq warning-minimum-level :emergency
      sentence-end-double-space nil
      kill-whole-line t
      jit-lock-defer-time 0
      display-line-numbers-type 'relative
      display-line-numbers-width-start t
      delete-by-moving-to-trash t
      use-dialog-box nil
      make-pointer-invisible t
      load-prefer-newer t
      max-lisp-eval-depth 10000
      max-specpdl-size 10000
      scroll-margin 8
      auto-revert-check-vc-info t
      scroll-step 1
      scroll-conservatively 101
      save-interprogram-paste-before-kill t
      isearch-lazy-count t
      isearch-yank-on-move 'shift
      isearch-allow-scroll 'unlimited
      isearch-repeat-on-direction-change t
      search-whitespace-regexp ".*?"
      indicate-buffer-boundaries t
      confirm-kill-emacs 'y-or-n-p)

(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

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
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-p"))

(pixel-scroll-precision-mode -1)
;; (mouse-avoidance-mode 'cat-and-mouse)
(column-number-mode 1)
(electric-pair-mode 1)
; (kill-ring-deindent-mode 1)
(context-menu-mode t)
(blink-cursor-mode -1)
(winner-mode 1)
(repeat-mode)
(delete-selection-mode 1)
(recentf-mode t)
(global-display-line-numbers-mode)
(global-auto-revert-mode 1)

(setq frame-title-format
      '(""
        (:eval
         (let ((project-name (nth 2 (project-current))))
           (unless (not project-name)
             (format "[%s] " project-name))))
        "%b"))


(defun ps/bookmark-save-no-prompt (&rest _)
  (funcall 'bookmark-save))
(advice-add 'bookmark-set-internal :after 'ps/bookmark-save-no-prompt)

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

(use-package no-littering :demand t)
(use-package general :demand t)
(use-package diminish :demand t)
(use-package delight :demand t)
(use-package hydra)
(use-package seq
  :preface (unload-feature 'seq t)
  :elpaca (seq :type git
               :host nil
               :repo "https://git.savannah.gnu.org/git/emacs/elpa.git"
               :branch "externals/seq"))

(elpaca-wait)

(use-package hl-line
  :elpaca nil
  :config
  (setq hl-line-sticky-flag nil)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode))

(use-package which-key
  :diminish
  :config
  (setq which-key-max-display-columns 5
        which-key-min-column-description-width 32
        which-key-add-column-padding 3
        which-key-max-description-length 32)
  (which-key-mode))

(use-package popup :defer t)
(use-package gcmh
  :demand t
  :diminish
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 100 1024 1024))
  (gcmh-mode 1))

(use-package bug-hunter :defer t)
(use-package restart-emacs :defer t)

(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package saveplace
  :elpaca nil
  :hook (after-init . save-place-mode))

(use-package project
  :elpaca nil
  :general (:keymaps 'project-prefix-map
                     "t" 'project-todo)
  :config
  (setq project-mode-line t
        project-file-history-behavior 'relativize)

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
  :elpaca nil
  :general
  (:keymaps 'ctl-x-map
            "C-b" nil
            "B" 'ibuffer-jump)
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

(use-package files
  :elpaca nil
  :config
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
        backup-by-copying t               ; Always use copying to create backup files
        delete-old-versions t             ; Delete excess backup versions
        kept-new-versions 6               ; Number of newest versions to keep when a new backup is made
        kept-old-versions 2               ; Number of oldest versions to keep when a new backup is made
        version-control t                 ; Make numeric backup versions unconditionally
        auto-save-default nil             ; Stop creating #autosave# files
        mode-require-final-newline nil    ; Don't add newlines at the end of files
        large-file-warning-threshold nil ; Open large files without requesting confirmation
        confirm-kill-processes nil
        create-lockfiles nil ; don't create .# files (crashes 'npm start')
        make-backup-files nil))

(use-package xref
  :elpaca nil
  :general
  ("C-M-," 'xref-go-forward)
  :config
  (add-hook 'xref-after-return-hook 'recenter)
  (add-hook 'xref-after-jump-hook 'recenter)
  (setq xref-history-storage 'xref-window-local-history))

(use-package which-function-mode
  :elpaca nil
  :hook ((prog-mode . which-function-mode))
  :init
  (setq which-func-unknown ""))

(use-package all-the-icons :defer t)
(use-package nerd-icons :defer t)

(use-package dired
  :elpaca nil
  :config
  (setq dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package all-the-icons-dired
  :after all-the-icons
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :elpaca nil
  :hook (after-init . savehist-mode)
  :init
  (setq savehist-additional-variables '(register-alist kill-ring)
        savehist-save-minibuffer-history t
        history-delete-duplicates t))

(use-package wgrep
  :defer t)

(use-package avy
  :general
  ("M-j" 'avy-goto-char-timer)
  ("M-n" 'avy-goto-line-below)
  ("M-p" 'avy-goto-line-above)
  (:keymaps 'isearch-mode-map
            "C-j" 'avy-isearch)
  :custom
  (avy-timeout-seconds 0.3))

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

(use-package treesit-jump
  :elpaca (:host github :repo "dmille56/treesit-jump" :files ("*.el" "treesit-queries"))
  :defer t
  :commands (treesit-jump-transient)
  :general ("C-c t" 'treesit-jump-transient))

(use-package autothemer)
(use-package kanagawa-theme
  :elpaca (:host github :repo "meritamen/emacs-kanagawa-theme"))

(use-package zenburn-theme)
(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-subtle-line-numbers t
        modus-themes-region '(bg-only no-extend)
        modus-themes-mode-line '(accented borderless)
        ;; modus-themes-hl-line '(accented)
        modus-themes-parens-match '(bold intense)))

(use-package color-theme-sanityinc-tomorrow)
(use-package color-theme-sanityinc-solarized
  :elpaca (:host github :repo "sudo-human/color-theme-sanityinc-solarized"))
(use-package afternoon-theme)
(use-package flatland-theme)
(use-package solarized-theme)
(use-package zeno-theme)
(use-package dracula-theme)
(use-package ef-themes)
(use-package lambda-themes
  :elpaca (:host github :repo "lambda-emacs/lambda-themes")
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
  :elpaca (:host github :repo "https://git.sr.ht/~ashton314/nordic-night" :branch "main"))
(use-package elune-theme)
(use-package gruber-darker-theme)
(use-package panda-theme)
(use-package wildcharm-theme)
(use-package wildcharm-light-theme)
(use-package nimbus-theme)
(use-package timu-macos-theme
  :config
  (customize-set-variable 'timu-macos-flavour "dark"))

(use-package emacs
  :elpaca nil
  :general
  (:keymaps 'ctl-x-map
            "2" 'split-and-follow-horizontally
            "3" 'split-and-follow-vertically)
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  ;; Add all your customizations prior to loading the themes

  ;; (defadvice load-theme (before clear-previous-themes activate)
  ;;   "Clear existing theme settings instead of layering them."
  ;;   (mapc #'disable-theme custom-enabled-themes))

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
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)



  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)
  :config
  (defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (select-window (get-lru-window)))

  (defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (select-window (get-lru-window))))

(add-hook 'elpaca-after-init-hook (lambda ()
                                    (load-theme 'modus-vivendi t)
                                    (load custom-file 'noerror)))

(use-package move-text
  :config
  (move-text-default-bindings)
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(use-package hl-todo
  :elpaca (hl-todo :host github
                   :repo "tarsius/hl-todo"
                   :version elpaca--latest-tag
                   :depth nil)
  :config
  (setq hl-todo-keyword-faces '(("TODO" . "#FF0000")
                                ("FIXME" . "#FF0000")
                                ("GOTCHA" . "#FF4500")
                                ("STUB" . "#1E90FF")
                                ("NOTE" . "#0090FF")
                                ("XXX" . "#AF0494")))
  (global-hl-todo-mode))

(use-package harpoon
  :custom
  (harpoon-project-package 'project)
  :init
  (define-prefix-command 'harpoon-map)
  (global-set-key (kbd "C-'") 'harpoon-map)
  :general (:keymaps 'harpoon-map
                     "h" 'harpoon-toggle-file
                     "'" 'harpoon-add-file
                     "c" 'harpoon-clear
                     "r" 'harpoon-toggle-quick-menu
                     "1" 'harpoon-go-to-1
                     "2" 'harpoon-go-to-2
                     "3" 'harpoon-go-to-3
                     "4" 'harpoon-go-to-4
                     "8" 'harpoon-go-to-5
                     "9" 'harpoon-go-to-6
                     "0" 'harpoon-go-to-7)
  :config
  (setq harpoon-cache-file (concat user-emacs-directory "harpoon/")))

;; (use-package rainbow-delimiters
;;   :defer t
;;   :hook ((prog-mode . rainbow-delimiters-mode)))


(use-package orderless
  :config
  (setq orderless-component-separator 'orderless-escapable-split-on-space
        completion-styles '(basic substring initials flex orderless)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-prefixes
                                    orderless-initialism
                                    orderless-flex))

;;   (defun orderless-company-completion (fn &rest args)
;;     "Highlight company matches correctly, and try default completion styles before
;; orderless."
;;     (let ((orderless-match-faces [completions-common-part])
;;           (completion-styles '(basic partial-completion orderless)))
;;       (apply fn args)))
;;   (advice-add 'company-capf--candidates :around 'orderless-company-completion)

  (orderless-define-completion-style orderless+basic
    (orderless-matching-styles '(orderless-literal
                                 orderless-regexp)))
  (setq completion-category-overrides
        '((command (styles orderless+basic))
          (symbol (styles orderless+basic))
          (variable (styles orderless+basic))
          (file (styles basic partial-completion)))))

(use-package vertico
  :elpaca (vertico :files (:defaults "extensions/*")
                   :includes (vertico-buffer
                              vertico-multiform
                              vertico-directory
                              vertico-flat
                              vertico-indexed
                              vertico-mouse
                              vertico-quick
                              vertico-repeat
                              vertico-reverse))

  :general (:keymaps 'vertico-map
            "M-j" #'vertico-quick-jump
            "<S-backspace>" #'vertico-directory-up)
  :init
  (setq vertico-count 7)
  (setq vertico-scroll-margin 1)
  (setq vertico-cycle t)

  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((consult-ripgrep buffer indexed)
          (consult-xref buffer indexed)
          (consult-imenu buffer)
          (xref-find-references buffer)))
  (setq vertico-multiform-categories
        '((consult-grep buffer)
          (imenu buffer))))

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :general ("M-A" 'marginalia-cycle)
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
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
         ("M-s d" . consult-fd)
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
         ("M-r" . consult-hqistory))                ;; orig. previous-matching-history-element

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
  (push "^*" consult-buffer-filter)

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
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)
   :preview-key '(:debounce 0.2 any))

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
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :after consult)

(use-package consult-git-log-grep
  :after consult
  :commands (consult-git-log-grep)
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package consult-todo
  :after (hl-todo consult))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-lsp
  :after (consult lsp))

(use-package embark
  :bind
  (("C-," . embark-act)
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

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

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


(use-package flycheck)
  ;; :config
  ;; (flycheck-define-checker python-ruff
  ;;                          "A super fast python linter Ruff!!!"
  ;;                          :command ("ruff-lsp" source)
  ;;                          :error-patterns
  ;;                          ((error line-start (file-name) ":" line ": error :" (message) line-end))
  ;;                          :modes (python-ts-mode python-mode)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package lsp-mode
  :init
  (setq lsp-idle-delay 0
        lsp-completion-provider :none
        lsp-file-watch-threshold 100000
        lsp-enable-snippet t
        lsp-keymap-prefix "C-c l")

  ;; (defun my/lsp-mode-setup-completion ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(orderless))) ;; Configure orderless

  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
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
  ;;               (setq my-flycheck-local-cache '((next-checkers . (python-ruff)))))))

  ;; (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)

  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))


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
;; (use-package lsp-treemacs)

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
  :hook ((python-ts-mode . pyvenv-mode))
  :config
  (add-hook 'pyvenv-post-activate-hooks
            #'(lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))))

  (add-hook 'pyvenv-post-deactivate-hooks
            #'(list (lambda ()
                      (setq python-shell-interpreter "python3")))))

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*"))
  :general (:keymaps 'corfu-map
                     "M-j" #'corfu-quick-jump
                     "M-d" #'corfu-popupinfo-toggle
                     "M-n" #'corfu-doc-scroll-up
                     "M-p" #'corfu-doc-scroll-down)

  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
  (corfu-preview-current t)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)
  (corfu-on-exact-match 'quit)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 3)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)

  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (and (local-variable-p 'completion-at-point-functions)
               (not (eq (current-local-map) read-passwd-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package emacs
  :elpaca nil
  :init
  (setq completion-cycle-threshold nil)
  
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package cape
  :hook corfu
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )


;; (use-package company
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-abort-on-unique-match nil)
;;   (company-show-quick-access t)
;;   (company-selection-wrap-around t)
;;   (company-tooltip-align-annotations t)
;;   (company-dabbrev-other-buffers nil)
;;   (company-dabbrev-downcase nil)
;;   (company-idle-delay 0.0)
;;   (compan-tooltip-idle-delay 0.1)
;;   (company-backends '(company-capf company-files company-yasnippet))
;;   (company-text-icons-add-background t)
;;   (company-format-margin-function #'company-text-icons-margin)
;;   (company-frontends '(company-pseudo-tooltip-frontend))
;;   (company-tooltip-minimum 8)
;;   :config
;;   (global-company-mode))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package transient
  :elpaca (:host github :repo "magit/transient")
  :defer t)

(use-package magit
  :defer t
  :after transient
  :config
  (magit-auto-revert-mode t))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook 'org-indent-mode)

  (defun org-mode-auto-fill-conf ()
    (progn
      (setq-local fill-column 100
                  auto-fill-function 'org-auto-fill-function)
      (auto-fill-mode +1)))
  (add-hook 'org-mode-hook 'org-mode-auto-fill-conf)

  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages '((restclient . t)))))

(use-package yaml
  :elpaca nil
  :mode ("\\.\\(yaml\\|yml\\)\\'" . yaml-ts-mode))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode))
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t))

(use-package git-review
  :elpaca (:repo "https://git.sr.ht/~niklaseklund/git-review"))

(use-package eshell
  :elpaca nil
  :defer t
  :general
  (eshell-mode-map
   "M-m" 'beginning-of-line))

(use-package vterm
  :defer t
  :commands vterm
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000))

(use-package eat
  :elpaca (:type git :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (setq eat-term-terminfo-directory eat--terminfo-path))

(use-package restclient
  :defer t
  :mode ("\\.rest\\'". restclient-mode)
  :config (add-hook 'restclient-mode-hook (lambda ()
                                            (setq imenu-generic-expression '((nil "^#+\s+.+" 0))))))

(use-package ob-restclient :after restclient)

(use-package sly
  :defer t
  :config
  (setq inferior-lisp-program "ros -Q run"))


(use-package sql-indent)

(use-package bigquery-mode
  :elpaca (:host github :repo "christophstockhusen/bigquery-mode"))

(use-package proced
  :elpaca nil
  :config
  (setq proced-enable-color-flag t))

(use-package redacted
  :config
  (add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1)))))


(use-package lemon
  :disabled t
  :elpaca (:type git :repo "https://codeberg.org/emacs-weirdware/lemon.git")
  :config
  (setq lemon-delay 5)
  (lemon-mode 1))

(use-package sokoban)
(use-package typit)
(use-package request)


;; My code
(setq sql-connection-alist
      '((local-db (sql-product 'mysql)
                  (sql-user "root")
                  (sql-password "9899")
                  (sql-server "127.0.0.1")
                  (sql-database "unittest_plutus_exchange")
                  (sql-port 3306))
        (stage-db (sql-product 'mysql)
                  (sql-user "root")
                  (sql-server "127.0.0.1")
                  (sql-password "um8r3774C0R9")
                  (sql-port 3307))))

(defun connect-to-database (label)
  "Connect to the database associated with the given LABEL."
  (interactive)
  (let ((product (car (cdr (assoc label sql-connection-alist)))))
    (setq sql-product product)
    (sql-connect label)))

(defun ps/mysql-local ()
  "Connect to local database."
  (interactive)
  (connect-to-database 'local-db))

(defun ps/mysql-stage ()
  "Connect to stage database."
  (interactive)
  (connect-to-database 'stage-db))

;; need to improve this
(defun ps/copy-line (arg)
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


(defun ps/todo-file ()
  "Open my TODO.org file"
  (interactive)
  (find-file "~/Notes/TODO.org"))


(defun ps/delete-this-file ()
  "Delete file for current file buffer. Does not prompt."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        (progn
          (delete-file filename t)
          (message "Deleted file %s" filename))
      (message "This buffer is not visiting an existing file."))))

(defun ps/google-current-word ()
  "Search the current word on Google using browse-url."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if word
        (browse-url (concat "https://www.google.com/search?q=" word))
      (message "No word found at point."))))

(defun ps/reverse-text (beg end)
  "Reverse characters between BEG and END"
  (interactive "r")
  (let ((region (buffer-substring beg end)))
    (delete-region beg end)
    (insert (nreverse region))))