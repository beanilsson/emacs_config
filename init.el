;;; package --- Summary

;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" default)))
 '(package-selected-packages
   (quote
    (green-phosphor-theme highlight-symbol flycheck js2-mode json-mode web-mode sass-mode yaml-mode use-package spike-theme smex smartparens restclient markdown-mode ido-vertical-mode flx-ido exec-path-from-shell coffee-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; Code:
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

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (exec-path-from-shell-initialize))

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

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

;(use-package web-mode
;  :ensure t
;  :config
;  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;  (setq web-mode-markup-indent-offset 2)
;  (setq web-mode-css-indent-offset 2)
;  (setq web-mode-code-indent-offset 2))

; Word highlighting
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol
  :config
  (require 'highlight-symbol))

;; Code completion for c/c++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


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
(load-theme 'green-phosphor t)

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
;(setq auto-mode-alist (cons '("\\.html" . web-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.coffee" . coffee-mode) auto-mode-alist))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" default)))
 '(package-selected-packages
   (quote
    (sass-mode yaml-mode use-package spike-theme smex smartparens restclient markdown-mode ido-vertical-mode flx-ido exec-path-from-shell coffee-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; React, eslint and babel setup
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use web-mode for .js files
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

;; require flycheck
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint in favour of eslint
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))

;; use eslint with web-mode for js files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customise flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setw-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))
