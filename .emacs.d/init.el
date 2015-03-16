;; stefanrajkovic-emacs-init --- Stefan Rajkovic's .emacs file

;;; Commentary:
;;; N/A

;;; Code:

;; establish ownership
(setq user-full-name "Stefan Rajkovic")
(setq user-mail-address "stefan1rajkovic@gmail.com")

;; cask! yay less downloading stuff manually
(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
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

;; UNCOMMENT FOR OCAML SETUP
;; ;; Ocaml setup
;; (add-to-list 'load-path "/Users/stefanrajkovic/.opam/system/share/tuareg")
;; (load "tuareg-site-file")

;; ;; Add opam emacs directory to the load-path
;; (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
;; (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; ;; Load merlin-mode
;; (require 'merlin)
;; (push "<SHARE_DIR>/emacs/site-lisp" load-path) ; directory containing merlin.el
;; (setq merlin-command "<BIN_DIR>/ocamlmerlin")  ; needed only if ocamlmerlin not already in your PATH
;; (autoload 'merlin-mode "merlin" "Merlin mode" t)
;; (add-hook 'tuareg-mode-hook 'merlin-mode)
;; (add-hook 'caml-mode-hook 'merlin-mode)
;; (setq merlin-command 'opam)

;; ;; Setup environment variables using opam
;; (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
;;   (setenv (car var) (cadr var)))

;; ;; Update the emacs path
;; (setq exec-path (append (parse-colon-path (getenv "PATH"))
;; 						(list exec-directory)))

;; ;; Update the emacs load path
;; (add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
;; 										  (getenv "OCAML_TOPLEVEL_PATH")))

;; ;; Automatically load utop.el
;; (autoload 'utop "utop" "Toplevel for OCaml" t)

(provide 'init)
;;; init.el ends here
