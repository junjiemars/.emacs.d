;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-autoload.el
;;;;
;; Commentary: run before conditional loading
;;;;

;;; `cls'

(autoload 'assoc** (v-home%> "config/cls"))
(autoload 'mapcar** (v-home%> "config/cls"))
(autoload 'remove-if* (v-home%> "config/cls"))
(autoload 'member-if* (v-home%> "config/cls"))
(autoload 'every* (v-home%> "config/cls"))
(autoload 'some* (v-home%> "config/cls"))
(autoload 'loop* (v-home%> "config/cls"))

;; end of `cls'

;;; `ed'

(autoload 'called-interactively-p* (v-home%> "config/ed") nil nil 'macro)
(autoload 'delete-line* (v-home%> "config/ed") nil nil 'macro)
(autoload 'file-in-dirs-p (v-home%> "config/ed"))
(autoload 'newline* (v-home%> "config/ed") nil t)
(autoload 'open-next-line (v-home%> "config/ed") nil t)
(autoload 'open-previous-line (v-home%> "config/ed") nil t)
(autoload 'delete-line* (v-home%> "config/ed") nil nil 'macro)
(autoload 'shell-format-buffer (v-home%> "config/ed") nil nil 'macro)
(autoload 'version-strncmp (v-home%> "config/ed"))

;; end of `ed'

;;; `marks'
(autoload '_mark_quoted_asymmetry@_ (v-home%> "config/marks"))
(autoload '_mark_quoted_symmetry@_ (v-home%> "config/marks"))
(autoload 'mark-thing (v-home%> "config/marks"))
(autoload 'symbol@ (v-home%> "config/marks"))


;; end of `marks'

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

;;; `ssh'

(autoload 'ssh-remote-p (v-home%> "config/ssh") nil nil 'macro)
(autoload 'ssh-remote->ids (v-home%> "config/ssh") nil nil 'macro)
(autoload 'ssh-remote->user@host (v-home%> "config/ssh"))

;; end of `ssh'

;;; `tags'

(autoload 'tags-spec->% (v-home%> "config/tags") nil nil 'macro)
(autoload 'make-c-tags (v-home%> "config/tags"))
(autoload 'make-dir-ctags (v-home%> "config/tags"))
(autoload 'make-dir-tags (v-home%> "config/tags") nil t)
(autoload 'mount-tags (v-home%> "config/tags") nil t)
(autoload 'unmount-tags (v-home%> "config/tags") nil t)

;; end of `tags'

;; end of on-autoload.el
