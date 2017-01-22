;; stefan-emacs-init --- Stefan Rajkovic's .emacs file

;;; Commentary:
;;; N/A

;;; Code:

;; establish ownership

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq user-full-name "Stefan Rajkovic")
(setq user-mail-address "stefan1rajkovic@gmail.com")

;; cask/pallet! yay less downloading stuff manually
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; for remote stuff
(setq tramp-default-method "ssh")

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

;; backup files and autosave files away from work tree
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 `((".*" . ,temporary-file-directory))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t))
 )

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-eslintrc "~/.eslintrc")
(defun my-flycheck-c-setup ()
  "Setup for flycheck and C."
  (interactive)
  (setq flycheck-clang-language-standard "gnu11")
  (setq flycheck-gcc-language-standard "gnu11")
  )
(add-hook 'c-mode-hook #'my-flycheck-c-setup)

;; whitespace cleanup on file saves/closes! :D
(require 'whitespace-cleanup-mode)
(setq-default show-trailing-whitespace t)

;; rainbow delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; column enforcing
(add-hook 'prog-mode-hook 'column-enforce-mode)

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
(define-key global-map "\M-*" 'pop-tag-mark)

;; Proper C setup
(require 'cc-mode)
(setq-default c-default-style "bsd"
              c-basic-offset 8
              tab-width 8
              indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(add-hook 'c-mode-common-hook
	  (lambda () (define-key c-mode-base-map (kbd "C-c C-l") 'compile)))

;; Org Mode
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(load "/Users/stefan/.opam/system/share/emacs/site-lisp/tuareg-site-file")

;; Add the opam lisp dir to the emacs load path
(add-to-list
 'load-path
 (replace-regexp-in-string
  "\n" "/share/emacs/site-lisp"
  (shell-command-to-string "opam config var prefix")))

;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)
(setq utop-command "opam config exec -- utop -emacs")
(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)
  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (pallet yaml-mode whitespace-cleanup-mode use-package tuareg smex rust-mode rainbow-delimiters markdown-mode flycheck-rust flycheck-ocaml flycheck-cask flx-ido exec-path-from-shell column-enforce-mode auto-complete utop))))

(provide 'init)
;;; init.el ends here
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
