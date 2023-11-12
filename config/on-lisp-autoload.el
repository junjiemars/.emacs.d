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
  (append! #'eldoc-mode emacs-lisp-mode-hook))



;; (with-eval-after-load 'scheme

;;   ;; disable auto active other scheme hooks
;;   (when-var% scheme-mode-hook 'scheme
;;     (setq scheme-mode-hook nil)))




(defun set-ielm-mode! ()
  (eldoc-mode)
  (when-lexical%
    (setq lexical-binding t)))

(with-eval-after-load 'ielm
  (when-var% ielm-mode-hook 'ielm
    (append! #'set-ielm-mode! ielm-mode-hook)))




(defmacro set-minibuffer-complete-key! (key)
  "Set completing KEY for `minibuffer'."
  `(define-key% (if-var% minibuffer-local-completion-map 'minibuffer
                         minibuffer-local-completion-map
                  minibuffer-local-map)
     (kbd ,key)
     (if-fn% 'minibuffer-complete 'minibuffer
             #'minibuffer-complete
       (if-fn% #'completion-at-point 'minibuffer
               #'completion-at-point
         #'lisp-complete-symbol))))


(defun minibuffer*-init! ()
  (set-minibuffer-complete-key! "TAB")
  (set-minibuffer-complete-key! "C-M-i"))



(make-thread* #'minibuffer*-init!)

;; end of on-lisp-autoload.el
