;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-lisp-autoload.el
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


;;; `elisp-mode' or `lisp-mode' after load
(eval-after-load (if-version% <= 25.0 'elisp-mode 'lisp-mode)
  #'on-elisp-init!)

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

;;; `ielm' after load
(eval-after-load 'ielm #'on-ielm-init!)

;; end of `ielm'


;;; `minibuffer'

(defmacro set-minibuffer-complete-key! (key)
  "Set completing KEY for `minibuffer'."
  `(define-key%
    (if-var% minibuffer-local-completion-map 'minibuffer
             minibuffer-local-completion-map
      minibuffer-local-map)
    (kbd ,key)
    (if-fn% 'minibuffer-complete 'minibuffer
            #'minibuffer-complete
      (if-fn% #'completion-at-point 'minibuffer
              #'completion-at-point
        #'lisp-complete-symbol))))


(defun minibuffer*-init! ()
  "Intialize \\=`minibuffer-mode\\='."
  (set-minibuffer-complete-key! "TAB")
  (set-minibuffer-complete-key! "C-M-i"))

(make-thread* #'minibuffer*-init!)

;; end of `minibuffer'


;; end of on-lisp-autoload.el
