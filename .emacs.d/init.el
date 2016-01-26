;; stefan-emacs-init --- Stefan Rajkovic's .emacs file

;;; Commentary:
;;; N/A

;;; Code:

;; establish ownership
(setq user-full-name "Stefan Rajkovic")
(setq user-mail-address "stefan1rajkovic@gmail.com")

;; cask! yay less downloading stuff manually
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; mouse stuff
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)
(global-set-key [mouse-4] (lambda ()
							(interactive)
							(scroll-down 1)))
(global-set-key [mouse-5] (lambda ()
							(interactive)
							(scroll-up 1)))

;; y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; backup files no longer
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(defun my-flycheck-c-setup ()
  "Setup for flycheck and C."
  (interactive)
  (setq flycheck-clang-language-standard "gnu11")
  (setq flycheck-gcc-language-standard "gnu11")
  )
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

;; whitespace cleanup on file saves/closes! :D
(require 'whitespace-cleanup-mode)

;; take PATH from shell, not defaults
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; ido
(require 'ido)
(require 'flx-ido)
(ido-mode t)
(flx-ido-mode t)

;; generic set up
(setq-default column-number-mode t
			  show-paren-mode t)
(electric-indent-mode 1)
(electric-pair-mode 1)
(show-paren-mode)
(auto-insert-mode)

;; Proper C setup
(require 'cc-mode)
(setq-default c-default-style "linux"
              c-basic-offset 4)
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(add-hook 'c-mode-common-hook
		  (lambda () (define-key c-mode-base-map (kbd "C-c C-l") 'compile)))

;; Org Mode
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; Rust
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Never ever use a tab
(setq-default indent-tabs-mode nil)

(provide 'init)
;;; init.el ends here
