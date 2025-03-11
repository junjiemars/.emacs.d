;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-org-autoload.el
;;;;

(autoload 'use-org-init! (v-home%> "config/use-org"))

(with-eval-after-load 'org-mode
  (make-thread* #'use-org-init!))



;; end of use-org-autoload.el
