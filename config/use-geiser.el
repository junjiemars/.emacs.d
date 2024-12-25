;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-geiser.el
;;;;

(defalias 'geiser*-lisp-implementations
  (let ((ls (let ((ns nil))
              (dolist (x ns)
                (let ((bin (executable-find* (symbol-name x))))
                  (when bin (push! x ns))))
              '(chicken guile racket))))
    (lambda (&optional new)
      (setq% geiser-active-implementations
             (if new (push! new ls) ls)
             geiser)))
  "Parameterized set `geiser-active-implementations'.")

(defun use-geiser-init! ()
  ;; builtin `chez-mode' and `gambit-mode' better than `geiser-mode'
  (geiser*-lisp-implementations)
  (setq% geiser-default-implementation
         (car geiser-active-implementations)
         geiser-mode))


(provide 'use-geiser)

;; end of use-geiser.el
