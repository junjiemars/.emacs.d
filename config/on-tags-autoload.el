;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-tags-autoload.el
;;;;

(autoload 'mount-tags (v-home%> "config/tags")
  "Mount tags." t)
(autoload 'unmount-tags (v-home%> "config/tags")
  "Unmount tags." t)
(autoload 'make-dir-tags (v-home%> "config/tags")
  "Make dir tags." t)
(autoload 'make-dir-ctags (v-home%> "config/tags"))

;; end of on-tags-autoload.el
