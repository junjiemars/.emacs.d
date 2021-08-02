;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-org-autoload.el
;;;;


(defmacro-if-feature% ox-reveal)


;; fix: Warning (bytecomp): `org-bookmark-jump-unhide' fn might not be
;; defined at runtime.
(when-fn% 'org-bookmark-jump-unhide 'org
  (autoload 'org-bookmark-jump-unhide "org"))


(with-eval-after-load 'org
  ;; load `ox-reveal' if it had been installed.
  ;; `ox-reveal' raising "package cl is deprecated".
  (when-feature-allowed% if-feature-ox-reveal%
    (when-var% org-reveal-root 'ox-reveal
      (require 'ox-reveal)
      (setq org-reveal-root
            (let ((root (emacs-home* ".reveal.js/")))
              (if (file-exists-p root)
                  root
                ;; "https://cdn.jsdelivr.net/reveal.js/3.8.0/"
                "https://pagecdn.io/lib/reveal/3.8.0/")))))

  ;; disable _ sub-superscripts
  (when-var% org-use-sub-superscripts 'org
    (setq org-use-sub-superscripts nil))

  ;; disable invisible edit
  (when-var% org-catch-invisible-edits 'org t)

  ;; define keys
  (define-key% (current-global-map) (kbd "C-c o l") #'org-store-link)
  (define-key% (current-global-map) (kbd "C-c o a") #'org-agenda)
  (when-fn% 'org-capture 'org
    (define-key% (current-global-map) (kbd "C-c o c") #'org-capture))
  (when-fn% 'org-switchb 'org
    (define-key% (current-global-map) (kbd "C-c o s") #'org-switchb)))


;; auto `org-mode'
(when-version% >= 23
  (push! (cons "\\.org\\'" 'org-mode) auto-mode-alist ))


;; end of on-org-autoload.el
