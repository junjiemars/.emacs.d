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




(defmacro minibuffer-complete-key! (key)
  "Complete KEY for `minibuffer' completion."
  `(define-key% (if-var% minibuffer-local-completion-map 'minibuffer
                         minibuffer-local-completion-map
                  minibuffer-local-map)
     (kbd ,key)
     (if-fn% 'minibuffer-complete 'minibuffer
             #'minibuffer-complete
       (if-fn% #'completion-at-point 'minibuffer
               #'completion-at-point
         #'lisp-complete-symbol))))

(minibuffer-complete-key! "TAB")
(minibuffer-complete-key! "C-M-i")




;; end of file
