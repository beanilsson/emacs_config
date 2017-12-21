;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ag
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-mode 1)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  ;; the old M-x
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")
  (if (fboundp 'global-flycheck-mode)
      (global-flycheck-mode +1)
    (add-hook 'prog-mode-hook 'flycheck-mode)))

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

; Show programming language icons instead of text
(use-package mode-icons-mode
  :ensure t
  :diminish mode-icons-mode
  :config
  (require 'mode-icons-mode))
; Word highlighting
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol
  :config
  (require 'highlight-symbol))
; Auto complete
(require 'auto-complete)
(global-auto-complete-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run Lisp under Emacs
(setq inferior-lisp-program "/usr/bin/clisp")

;; customize minibuffer startup message
(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(defun display-startup-echo-area-message ()
  "Customize minibuffer startup message."
  (message "Hack away, Master %s!" current-user))

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable scroll bar
(scroll-bar-mode -1)

;; disable blinking cursor
(blink-cursor-mode -1)

;; disable bell ring
(setq ring-bell-function 'ignore)

;; highlight the current line
(global-hl-line-mode +1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; map meta to command key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; bind commenting
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; bind cleaning
(global-set-key (kbd "C-c n") 'simple-clean-region-or-buffer)

;; do whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; no tabs
(setq-default indent-tabs-mode nil)

;; no backups
(setq make-backup-files nil)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; set font size (value is in 1/10pt, so 100 will give you 10pt)
(set-face-attribute 'default nil :height 160)

;; show matching parens
(show-paren-mode t)

;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; stop Ruby mode from auto-inserting encoding comment
(setq ruby-insert-encoding-magic-comment nil)

;; utf-8 always and forever
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean region or buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-clean-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun simple-clean-region-or-buffer ()
  "Cleans a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (whitespace-cleanup)
          (message "Cleaned selected region"))
      (progn
        (simple-clean-buffer)
        (whitespace-cleanup)
        (message "Cleaned buffer")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'spike t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; webmode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil :height 140)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; associate files with modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist (cons '("\\.rest" . restclient-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sass" . sass-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.scss" . scss-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ex" . elixir-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.exs" . elixir-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yml" . yaml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.hbs" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.coffee" . coffee-mode) auto-mode-alist))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (sass-mode yaml-mode web-mode use-package spike-theme smex smartparens restclient markdown-mode ido-vertical-mode flycheck flx-ido exec-path-from-shell coffee-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x g") 'magit-git-command)

;; js lint

(require 'json)
(require 'flycheck)
(defun my-parse-jslinter-warning (warning)
  (flycheck-error-new
   :line (1+ (cdr (assoc 'line warning)))
   :column (1+ (cdr (assoc 'column warning)))
   :message (cdr (assoc 'message warning))
   :level 'error
   :buffer (current-buffer)
   :checker 'javascript-jslinter))
(defun jslinter-error-parser (output checker buffer)
  (mapcar 'parse-jslinter-warning
          (cdr (assoc 'warnings (aref (json-read-from-string output) 0)))))
(flycheck-define-checker javascript-jslinter
  "A JavaScript syntax and style checker based on JSLinter.

See URL `https://github.com/tensor5/JSLinter'."
  :command ("c:/Users/bea//usr/local/lib/node_modules/jslint" "--raw" source)
  :error-parser jslinter-error-parser
  :modes (js-mode js2-mode js3-mode))
