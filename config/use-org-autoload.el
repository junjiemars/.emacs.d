;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-org-autoload.el
;;;;

;; (defun use-org-init! ())
(setq% org-reveal-root
       (let ((root (emacs-home% ".reveal.js/")))
         (if (file-exists-p root)
             root
           "https://cdn.jsdelivr.net/npm/reveal.js"))
       ox-reveal)



;; end of use-org-autoload.el
