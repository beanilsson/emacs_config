;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Map meta to Mac command key.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; No splash.
(setq inhibit-splash-screen t)

;; No menu.
(menu-bar-mode -1)

;;Remove Scrollbar
(scroll-bar-mode -1)

;; No tabs.
(setq-default indent-tabs-mode nil)

;; No backups.
(setq make-backup-files nil)

;;disable auto save
(setq auto-save-default nil)

;; Show column number.
(column-number-mode t)

;; Highlight current line.
(global-hl-line-mode t)

;; Show matching parens.
(show-paren-mode t)

;; Delete trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; y or n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Bind commenting.
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Bind cleaning.
(global-set-key (kbd "C-c n") 'simple-clean-region-or-buffer)

;; stop Ruby mode from auto-inserting encoding comment
(setq ruby-insert-encoding-magic-comment nil)

;; auto reload buffer when changed on disc
(global-auto-revert-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; git status
(global-set-key (kbd "C-x g") 'magit-status)

;; git diff
(global-set-key (kbd "C-x d") 'magit-diff)

;; git log
(global-set-key (kbd "C-x l") 'magit-log)

;; git pull
(global-set-key (kbd "C-x p l") 'magit-pull)

;; git push
(global-set-key (kbd "C-x p u") 'magit-push)

;; git commit
(global-set-key (kbd "C-x c o") 'magit-commit)

;; git commit amend
(global-set-key (kbd "C-x c a") 'magit-commit-amend)

;; git checkout
(global-set-key (kbd "C-x c h") 'magit-checkout)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean region or buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-indent-buffer ()
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
          (message "Cleaned selected region."))
      (progn
        (simple-indent-buffer)
        (whitespace-cleanup)
        (message "Cleaned buffer.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List packages to install if missing.
(setq package-list '(flycheck restclient))

;; List repositories containing the packages.
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; Load installed packages.
(package-initialize)

;; Fetch list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Avoid loading installed packages again after processing the init file.
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git status in bottom bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
  (setq ad-return-value
    (concat ad-return-value
            (let ((plus-minus (vc-git--run-command-string
                               file "diff" "--numstat" "--")))
              (and plus-minus
                   (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                   (format " +%s-%s" (match-string 1 plus-minus) (match-string 2 plus-minus)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git latest commit author in bottom bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice vc-git-mode-line-string (after author (file) compile activate)
  (setq ad-return-value
        (concat ad-return-value
                (let ((author
                       (vc-git--run-command-string file "log" "-n1" "--pretty=format:%an" "--")))
                  (when author
                    (concat " " author))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

package-enable-at-startup
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;;Ido vertical mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-x") 'smex)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(load-theme 'gotham t)
;;(load-theme 'darktooth t)
;(load-theme 'afternoon t)
(load-theme 'spike t)


;;disable the merging (the "looking in other directories" in ido vulgo) with

(setq ido-auto-merge-work-directories-length -1)

;;Update Package List
(when (not package-archive-contents)
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rspec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'load-path "/path/to/rspec-mode")
(require 'rspec-mode)

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
(setq auto-mode-alist (cons '("\\.coffee" . coffee-mode) auto-mode-alist))
