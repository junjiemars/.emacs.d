;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-org.el
;;;;

;;; require

;; end of require

(defun use-org-init! ()
  "On \\=`org\\=' initialization."
  (setq% org-reveal-root
         (let ((root (emacs-home% ".reveal.js/")))
           (if (file-exists-p root)
               root
             "https://cdn.jsdelivr.net/npm/reveal.js"))
         ox-reveal))



(provide 'use-org)

;; end of use-org.el
