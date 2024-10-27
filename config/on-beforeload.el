;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-autoload.el
;;;;
;; Commentary: run before conditional loading
;;;;

;;; `ed'

(autoload 'delete-line* (v-home%> "config/ed") nil nil 'macro)
(autoload 'newline* (v-home%> "config/ed") nil t)
(autoload 'shell-format-buffer (v-home%> "config/ed") nil nil 'macro)
(autoload 'symbol@ (v-home%> "config/ed") nil nil 'macro)

;; end of `ed'

;;; `mill'

(autoload 'kill-quoted-asymmetry@ (v-home%> "config/mill") nil t)
(autoload 'kill-quoted-symmetry@ (v-home%> "config/mill") nil t)
(autoload 'kill-sexp@ (v-home%> "config/mill") nil t)
(autoload 'kill-string@ (v-home%> "config/mill") nil t)
(autoload 'kill-word@ (v-home%> "config/mill") nil t)
(autoload 'mark-defun@ (v-home%> "config/mill") nil t)
(autoload 'mark-filename@ (v-home%> "config/mill") nil t)
(autoload 'mark-line@ (v-home%> "config/mill") nil t)
(autoload 'mark-quoted-asymmetry@ (v-home%> "config/mill") nil t)
(autoload 'mark-quoted-symmetry@ (v-home%> "config/mill") nil t)
(autoload 'mark-sexp@ (v-home%> "config/mill") nil t)
(autoload 'mark-string@ (v-home%> "config/mill") nil t)
(autoload 'mark-word@ (v-home%> "config/mill") nil t)

;; end of `mill'

;;; `tags'

(autoload 'make-c-tags (v-home%> "config/tags"))
(autoload 'make-dir-ctags (v-home%> "config/tags"))
(autoload 'make-dir-tags (v-home%> "config/tags") nil t)
(autoload 'mount-tags (v-home%> "config/tags") nil t)
(autoload 'unmount-tags (v-home%> "config/tags") nil t)

;; end of `tags'

;; end of on-autoload.el
