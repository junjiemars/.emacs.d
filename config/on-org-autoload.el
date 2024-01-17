;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-org-autoload.el
;;;;

(declare-function on-org-init! (v-home%> "config/orgs"))
(autoload 'on-org-init! (v-home%> "config/orgs"))


;;; `org' after load
(with-eval-after-load 'org
  (on-org-init!))

;; end of macro

;; auto `org-mode'
(when-version% >= 23
  (push! (cons "\\.org\\'" 'org-mode) auto-mode-alist ))


;; end of on-org-autoload.el
