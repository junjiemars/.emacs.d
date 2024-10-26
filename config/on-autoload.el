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

(autoload 'shell-format-buffer (v-home%> "config/ed") nil nil 'macro)
(autoload 'symbol@ (v-home%> "config/ed") nil nil 'macro)
(autoload 'delete-line* (v-home%> "config/ed") nil nil 'macro)

;; end of `ed'

;;; `tags'

(autoload 'mount-tags (v-home%> "config/tags") nil t)
(autoload 'unmount-tags (v-home%> "config/tags") nil t)
(autoload 'make-dir-tags (v-home%> "config/tags") nil t)
(autoload 'make-dir-ctags (v-home%> "config/tags"))

;; end of `tags'

;; end of on-autoload.el
