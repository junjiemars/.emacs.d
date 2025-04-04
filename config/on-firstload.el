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
(autoload 'drop (v-home%> "config/cls") nil nil 'macro)
(autoload 'every* (v-home%> "config/cls"))
(autoload 'insert! (v-home%> "config/cls") nil nil 'macro)
(autoload 'loop* (v-home%> "config/cls"))
(autoload 'mapcar** (v-home%> "config/cls"))
(autoload 'member-if* (v-home%> "config/cls"))
(autoload 'remove-if* (v-home%> "config/cls"))
(autoload 'some* (v-home%> "config/cls"))
(autoload 'take* (v-home%> "config/cls") nil nil 'macro)
(autoload 'take-while (v-home%> "config/cls"))

;; end of `cls'

;;; `ed'

(autoload 'delete-line* (v-home%> "config/ed"))
(autoload 'file-in-dirs-p (v-home%> "config/ed"))
(autoload 'newline* (v-home%> "config/ed") nil t)
(autoload 'open-next-line (v-home%> "config/ed") nil t)
(autoload 'open-previous-line (v-home%> "config/ed") nil t)
(autoload 'vstrncmp (v-home%> "config/ed"))

;; end of `ed'

;;; `marks'
(autoload 'mark-thing (v-home%> "config/marks"))
(autoload 'symbol@ (v-home%> "config/marks"))
(autoload 'kill-quoted-asymmetry@ (v-home%> "config/marks") nil t)
(autoload 'kill-quoted-symmetry@ (v-home%> "config/marks") nil t)
(autoload 'kill-sexp@ (v-home%> "config/marks") nil t)
(autoload 'kill-string@ (v-home%> "config/marks") nil t)
(autoload 'kill-word@ (v-home%> "config/marks") nil t)
(autoload 'mark-defun@ (v-home%> "config/marks") nil t)
(autoload 'mark-filename@ (v-home%> "config/marks") nil t)
(autoload 'mark-line@ (v-home%> "config/marks") nil t)
(autoload 'mark-quoted-asymmetry@ (v-home%> "config/marks") nil t)
(autoload 'mark-quoted-symmetry@ (v-home%> "config/marks") nil t)
(autoload 'mark-sexp@ (v-home%> "config/marks") nil t)
(autoload 'mark-string@ (v-home%> "config/marks") nil t)
(autoload 'mark-symbol@ (v-home%> "config/marks") nil t)
(autoload 'mark-word@ (v-home%> "config/marks") nil t)

;; end of `marks'

;;; `ssh'

(autoload 'ssh-remote-p (v-home%> "config/ssh"))
(autoload 'ssh-remote->ids (v-home%> "config/ssh"))
(autoload 'ssh-remote->user@host (v-home%> "config/ssh"))

;; end of `ssh'

;;; `tags'

(autoload 'tags-spec->* (v-home%> "config/tags"))
(autoload 'make-c-tags (v-home%> "config/tags"))
(autoload 'make-dir-ctags (v-home%> "config/tags"))
(autoload 'make-dir-tags (v-home%> "config/tags") nil t)
(autoload 'mount-tags (v-home%> "config/tags") nil t)
(autoload 'unmount-tags (v-home%> "config/tags") nil t)

;; end of `tags'

;; end of on-autoload.el
