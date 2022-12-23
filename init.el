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
   '("583148e87f779040b5349db48b6fcad6fe9a873c6ada20487e9a1ec40d845505" "44247f2a14c661d96d2bff302f1dbf37ebe7616935e4682102b68c0b6cc80095" default))
 '(package-selected-packages
   '(highlight-symbol js-comint move-text run-import-js import-js tide typescript-mode fold-this yafolding-mode ac-js2 auto-complete magit flymake-eslint add-node-modules-path web-mode prettier-js prettier ## auto-complete-mode yafolding sass-mode yaml-mode use-package spike-theme smex smartparens restclient markdown-mode ido-vertical-mode flx-ido exec-path-from-shell coffee-mode ag)))
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
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  )

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

(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol
  :config
  (require 'highlight-symbol))

(require 'yafolding)

(use-package yafolding
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . yafolding-mode)))

(require 'fold-this)
(global-set-key (kbd "C-c C-f") 'fold-this-all)
(global-set-key (kbd "C-c C-F") 'fold-this)
(global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

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

;; move-text bindings
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; yafolding bindings
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-q-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))

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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

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
                                        ;(require 'auto-complete)

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (defvar web-mode-markup-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-init-hook)

(require 'prettier-js)
(add-hook 'web-mode-hook 'prettier-js-mode)

;; Force single quotes and trailing comma
(setq prettier-js-args '(
                         "--single-quote" "true"
                         "--trailing-comma" "all"
))

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
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))

;;use local eslint from node_modules before global
;;http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
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
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))

(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))

;; for better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; auto complete js
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
(ac-config-default)

;; tsx
  (require 'web-mode)

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  ;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; ts
  (require 'web-mode)

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "ts" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  ;; enable typescript - tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)

;; import-js
(require 'import-js)
(require 'grizzl)

;; tide
  (use-package tide :ensure t)
  (use-package company :ensure t)
  (use-package flycheck :ensure t)

  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; js-comint
(require 'js-comint)

;;; init.el ends here
