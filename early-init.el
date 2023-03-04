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