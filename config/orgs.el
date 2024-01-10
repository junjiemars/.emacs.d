;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; orgs.el
;;;;

;;; macro

;; fix: Warning (bytecomp): `org-bookmark-jump-unhide' fn
;; might not be defined at runtime.
(when-fn% 'org-bookmark-jump-unhide 'org
  (autoload 'org-bookmark-jump-unhide "org"))

(defmacro-if-feature% ox-reveal)

(defmacro when-feature-ox-reveal% (&rest body)
  (declare (indent 0))
  (if-feature-ox-reveal%
      `(progn% ,@body)
    `(comment ,@body)))

;; end of macro

(defun on-org-init! ()
  "On \\=`org-mode\\=' initialization."
  ;; load `ox-reveal' if it had been installed.
  ;; `ox-reveal' raising "package cl is deprecated".
  (when-feature-ox-reveal%
    (when-var% org-reveal-root 'ox-reveal
      (require 'ox-reveal)
      (setq org-reveal-root
            (let ((root (emacs-home* ".reveal.js/")))
              (if (file-exists-p root)
                  root
                ;; "https://cdn.jsdelivr.net/reveal.js/3.8.0/"
                "https://pagecdn.io/lib/reveal/3.8.0/")))))
  ;; disable _ sub-superscripts
  (setq% org-use-sub-superscripts nil 'org)
  ;; define keys
  (define-key% (current-global-map) (kbd "C-c o l") #'org-store-link)
  (define-key% (current-global-map) (kbd "C-c o a") #'org-agenda)
  (when-fn% 'org-capture 'org
    (define-key% (current-global-map) (kbd "C-c o c") #'org-capture))
  (when-fn% 'org-switchb 'org
    (define-key% (current-global-map) (kbd "C-c o s") #'org-switchb)))



(provide 'orgs)

 ;; end of orgs.el
