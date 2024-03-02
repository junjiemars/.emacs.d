;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-tags-autoload.el
;;;;

(autoload 'tags-spec->% (v-home%> "config/tags")
  "Tags spec" nil 'macro)
(autoload 'tags-history (v-home%> "config/tags")
  "Load tags option or skip history.")
(autoload 'mount-tags (v-home%> "config/tags")
  "Mount tags." t)
(autoload 'unmount-tags (v-home%> "config/tags")
  "Unmount tags." t)
(autoload 'make-c-tags (v-home%> "config/tags")
  "Make C tags.")
(autoload 'tags-in-view-mode (v-home%> "config/tags"))


;; end of on-tags-autoload.el
