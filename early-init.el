(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors nil)
(setq native-comp-deferred-compilation-deny-list nil)
(setq package-enable-at-startup nil)
;; Set gc-cons-threshold to a higher value: 100mb
;; (setq gc-cons-threshold (* 100 1024 1024))

;; Bump up gc threshold until emacs startup
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1)))

;; Set window size to a sensable default
(add-to-list 'default-frame-alist '(height . 42))
(add-to-list 'default-frame-alist '(width . 124))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L200
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L323
(setq frame-inhibit-implied-resize t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L331
(setq inhibit-compacting-font-caches t)

;; https://github.com/hlissner/doom-emacs/blob/58af4aef56469f3f495129b4e7d947553f420fca/core/core.el#L205
(setq idle-update-delay 1.0)

;; Don't want a mode line while loading init.
(setq mode-line-format nil)

;; No scrollbar by default.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No nenubar by default.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; No toolbar by default.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; No tooltip by default.
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; No Alarms by default.
(setq ring-bell-function 'ignore
      visual-bell t)

(defun my/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/.emacs.d/emacs.png")
           (image (create-image image-path))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (prompt-title "Welcome to Emacs!"))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
      (insert prompt-title))
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(when (< (length command-line-args) 2)
  (add-hook 'emacs-startup-hook (lambda ()
                                  (when (display-graphic-p)
                                    (my/show-welcome-buffer)))))