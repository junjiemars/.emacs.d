;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; orgs.el
;;;;

;;; require

;; end of require

(defun on-org-init! ()
  "On \\=`org-mode\\=' initialization."
  ;; fix: Warning (bytecomp): `org-bookmark-jump-unhide' fn
  ;; might not be defined at runtime.
  (when-fn% org-bookmark-jump-unhide org
    (declare-function org-bookmark-jump-unhide "org")
    (autoload 'org-bookmark-jump-unhide "org"))
  ;; disable _ sub-superscripts
  (setq% org-use-sub-superscripts nil org)
  ;; define keys
  (define-global-key% "ol" #'org-store-link)
  (define-global-key% "oa" #'org-agenda)
  (when-fn% org-capture org
    (define-global-key% "oc" #'org-capture))
  (when-fn% org-switchb org
    (define-global-key% "os" #'org-switchb)))



(provide 'orgs)

 ;; end of orgs.el
