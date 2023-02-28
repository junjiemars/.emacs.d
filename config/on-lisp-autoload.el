;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-lisp-autoload.el
;;;;


;;; `elisp-mode'
(with-eval-after-load (if-version% <= 25.0 'elisp-mode 'lisp-mode)

  ;; safe local variables
  (safe-local-variable* 'Syntax)
  (safe-local-variable* 'Base)
  (safe-local-variable* 'Package)

  ;; `eldoc-mode'
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))



;; (with-eval-after-load 'scheme

;;   ;; disable auto active other scheme hooks
;;   (when-var% scheme-mode-hook 'scheme
;;     (setq scheme-mode-hook nil)))




(defun set-ielm-mode! ()
  (eldoc-mode)
  (when-lexical%
    (setq lexical-binding t)))

(with-eval-after-load 'ielm
  (add-hook 'ielm-mode-hook #'set-ielm-mode!))




;; fix: no TAB completion in minibuffer on ancient Emacs.
(if-key% minibuffer-local-map
    (kbd "TAB")
    (lambda (def) (memq def '(self-insert-command)))
    (progn
      (defun minibuffer-tab-completion! ()
        "TAB as completion key in minibuffer."
        ;; `lisp-complete-symbol' is an obsolete since Emacs24.4
        (define-key minibuffer-local-map (kbd "TAB")
          (if-fn% 'completion-at-point 'minibuffer
                  (if-version% > 24
                               #'lisp-complete-symbol
                    #'completion-at-point)
            #'lisp-complete-symbol)))
      (add-hook 'minibuffer-setup-hook #'minibuffer-tab-completion! t)))




;; end of file
