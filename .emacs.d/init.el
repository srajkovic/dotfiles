;; stefan-emacs-init --- Stefan Rajkovic's .emacs file

;;; Commentary:
;;; N/A

;;; Code:

;; establish ownership
(setq user-full-name "Stefan Rajkovic")
(setq user-mail-address "stefan1rajkovic@gmail.com")

;; cask! yay less downloading stuff manually
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
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

;; Rust
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Added by CS 51 setup script -- Tuareg
(load "~/.opam/4.02.3/share/emacs/site-lisp/tuareg-site-file")
(add-to-list 'load-path "~/.opam/system/share/emacs/site-lisp/")

;; Added by CS 51 setup script -- Merlin
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; Load merlin-mode
(require 'merlin)
;; Start merlin on ocaml files
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
;; Enable auto-complete. This only enables 'M-x auto-complete'.
;; For live autocomplete as you type, replace "'easy" with "t"
(setq merlin-ac-setup t)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)

(add-hook 'tuareg-mode-hook #'merlin-mode)
;; Added by CS 51 setup script -- UTOP
;; Setup environment variables using opam
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; Update the emacs path
(setq exec-path (append (parse-colon-path (getenv "PATH"))
                        (list exec-directory)))

;; Update the emacs load path
(add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                          (getenv "OCAML_TOPLEVEL_PATH")))

;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml"t)

(autoload 'utop-minor-mode "utop" "Minor mode for utop"t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; Rebinding some of Tuareg's bindings to use utop instead of the ocaml toplevel
(defun tuareg-mode-keybindings ()
  "Shadow a few of Tuareg mode's keybindings to use utop instead of the default toplevel."
  (interactive)
  (local-unset-key (kbd "C-c C-e"))
  (local-set-key (kbd "C-c C-e") 'utop-eval-phrase)
  (local-unset-key (kbd "C-c C-r"))
  (local-set-key (kbd "C-c C-r") 'utop-eval-region))
(add-hook 'tuareg-mode-hook 'tuareg-mode-keybindings)

(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(provide 'init)
;;; init.el ends here
