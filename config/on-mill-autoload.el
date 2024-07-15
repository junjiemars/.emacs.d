;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mill-autoload.el
;;;;
;; Commentary:
;;;;

(autoload 'kill-word@ (v-home%> "config/mill") nil t)
(autoload 'kill-sexp@ (v-home%> "config/mill") nil t)
(autoload 'kill-whole-line@ (v-home%> "config/mill") nil t)
(autoload 'kill-quoted-symmetry@ (v-home%> "config/mill") nil t)
(autoload 'kill-quoted-asymmetry@ (v-home%> "config/mill") nil t)
(autoload 'kill-string@ (v-home%> "config/mill") nil t)
(autoload 'mark-word@ (v-home%> "config/mill") nil t)
(autoload 'mark-sexp@ (v-home%> "config/mill") nil t)
(autoload 'mark-defun@ (v-home%> "config/mill") nil t)
(autoload 'mark-filename@ (v-home%> "config/mill") nil t)
(autoload 'mark-line@ (v-home%> "config/mill") nil t)
(autoload 'mark-quoted-symmetry@ (v-home%> "config/mill") nil t)
(autoload 'mark-quoted-asymmetry@ (v-home%> "config/mill") nil t)
(autoload 'mark-string@ (v-home%> "config/mill") nil t)

;; Kill
(define-key% (current-global-map) (kbd "C-x M-d") #'kill-word@)
(define-key% (current-global-map) (kbd "C-x M-e") #'kill-sexp@)
(define-key% (current-global-map) (kbd "C-x M-l") #'kill-whole-line)
(define-key% (current-global-map) (kbd "C-x M-q s") #'kill-quoted-symmetry@)
(define-key% (current-global-map) (kbd "C-x M-q a") #'kill-quoted-asymmetry@)
(define-key% (current-global-map) (kbd "C-x M-s") #'kill-string@)
;; Mark
(define-key% (current-global-map) (kbd "C-c M-@") #'mark-word@)
(define-key% (current-global-map) (kbd "C-c C-M-@") #'mark-sexp@)
(define-key% (current-global-map) (kbd "C-c M-h") #'mark-defun@)
(define-key% (current-global-map) (kbd "C-c M-f") #'mark-filename@)
(define-key% (current-global-map) (kbd "C-c M-l") #'mark-line@)
(define-key% (current-global-map) (kbd "C-c M-q s") #'mark-quoted-symmetry@)
(define-key% (current-global-map) (kbd "C-c M-q a") #'mark-quoted-asymmetry@)
(define-key% (current-global-map) (kbd "C-c M-s") #'mark-string@)
(define-key% (current-global-map) (kbd "M-@") #'mark-word)
(define-key% (current-global-map) (kbd "C-M-@") #'mark-sexp)
(define-key% (current-global-map) (kbd "C-M-SPC") #'mark-sexp)
(define-key% (current-global-map) (kbd "C-M-h") #'mark-defun)

;; end of on-mill-autoload.el
