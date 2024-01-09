;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-org-autoload.el
;;;;

(declare-function on-org-init! (v-home%> "config/orgs.el"))
(autoload 'on-org-init! (v-home%> "config/orgs.el"))


;;; `org' after load
(with-eval-after-load 'org
  (on-org-init!))

;; fix: Warning (bytecomp): `org-bookmark-jump-unhide' fn
;; might not be defined at runtime.
(when-fn% 'org-bookmark-jump-unhide 'org
  (autoload 'org-bookmark-jump-unhide "org"))

;; end of macro

;; auto `org-mode'
(when-version% >= 23
  (push! (cons "\\.org\\'" 'org-mode) auto-mode-alist ))


;; end of on-org-autoload.el
