;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; lisps.el
;;;;


;;; `elisp-mode'

(defun on-elisp-init! ()
  "On \\=`elisp-mode\\=' initialization."
  ;; safe local variables
  (safe-local-variable* 'Syntax)
  (safe-local-variable* 'Base)
  (safe-local-variable* 'Package)
  ;; `eldoc-mode'
  (append! #'eldoc-mode emacs-lisp-mode-hook))

;; end of `elisp-mode'


;;; `scheme'

;; (with-eval-after-load 'scheme

;;   ;; disable auto active other scheme hooks
;;   (when-var% scheme-mode-hook 'scheme
;;     (setq scheme-mode-hook nil)))

;; end of `scheme'


;;; `ielm'

(defun set-ielm-mode! ()
  (eldoc-mode)
  (when-lexical%
    (setq lexical-binding t)))

(defun on-ielm-init! ()
  "On \\=`ielm\\=' initialization."
  (when-var% ielm-mode-hook 'ielm
    (append! #'set-ielm-mode! ielm-mode-hook)))

;; end of `ielm'

(provide 'lisps)

;; end of lisps.el
