;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-geiser-autoload.el
;;;;
;; chez
;;; https://scheme.com
;;; http://cisco.github.io/ChezScheme/csug9.5/csug.html
;;;
;; racket : https://racket-lang.org
;;;

(defmacro-if-feature% geiser)

;;; Disable auto active `geiser-mode'
(fset 'geiser-mode--maybe-activate nil)

;;; Disable auto `geiser-mode' for `scheme-mode'
(when-var% geiser-mode-auto-p 'geiser-mode
  (setq% geiser-mode-auto-p nil 'geiser-mode))


(defalias '*geiser-lisp-implementations*
  (lexical-let% ((ls (let ((ns))
                       (dolist* (x ns)
                         (let ((bin (executable-find (symbol-name x))))
                           (when bin (push x ns))))
                       '(chicken guile racket))))
    (lambda (&optional new)
      (setq% geiser-active-implementations
             (if new (push new ls) ls)
             'geiser)))
  "Parameterized set `geiser-active-implementations'.")


(with-eval-after-load 'geiser
  ;;; builtin `chez-mode' and `gambit-mode' better than `geiser'
  (*geiser-lisp-implementations*)
  (setq% geiser-default-implementation
         (car geiser-active-implementations)
         'geiser-mode))


;; eof
