;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; treesits.el
;;;;

(require 'treesit)

(defalias 'treesit*-recipe
  (let ((b (v-home% ".exec/treesit-settings.el"))
        (m '((:lang cpp
                    :mode c++-mode
                    :url "https://github.com/tree-sitter/tree-sitter-cpp"
                    :map c++-ts-mode)
             (:lang c
                    :mode c-mode
                    :url "https://github.com/tree-sitter/tree-sitter-c"
                    :map c-ts-mode)
             (:lang c-or-c++
                    :mode c-or-c++-mode
                    :map c-or-c++-ts-mode)
             (:lang python
                    :mode python-mode
                    :url "https://github.com/tree-sitter/tree-sitter-python"
                    :map python-ts-mode))))
    (lambda (&optional op sexp)
      (cond ((and op (eq op :put)) (dolist (x sexp) (push! x m t)))
            ((and op (eq op :read)) (setq m (read-sexp-from-file b)))
            ((and op (eq op :dump)) (when m (save-sexp-to-file m b)))
            (t m))))
  "The \\=`treesit\\=' recipe.")

(defun treesit*-on/off-check (recipe)
  "Return t if \\=`treesit\\=' is on, otherwise nil."
  (catch 'br
    (dolist (x recipe)
      (let ((m (plist-get x :map)))
        (dolist (a major-mode-remap-alist)
          (and (eq m (cdr a)) (throw 'br t)))
        (dolist (a auto-mode-alist)
          (and (eq m (cdr a)) (throw 'br t)))))))

(defun toggle-treesit! ()
  "Toggle \\=`treesit\\=' on or off."
  (interactive)
  (let* ((ts (treesit*-recipe))
         (on (treesit*-on/off-check ts)))
    (cond (on (setq auto-mode-alist
                    (let ((as nil))
                      (dolist (a auto-mode-alist (nreverse as))
                        (unless (catch 'br
                                  (dolist (x ts)
                                    (and (eq (plist-get x :map) (cdr a))
                                         (throw 'br t))))
                          (setq as (cons a as)))))
                    major-mode-remap-alist
                    (let ((as nil))
                      (dolist (a major-mode-remap-alist (nreverse as))
                        (unless (catch 'br
                                  (dolist (x ts)
                                    (and (eq (plist-get x :map) (cdr a))
                                         (throw 'br t))))
                          (setq as (cons a as)))))
                    treesit-language-source-alist nil))
          (t (let ((aa (copy-sequence auto-mode-alist)))
               (dolist (x ts)
                 (let ((l1 (plist-get x :lang))
                       (m1 (plist-get x :mode))
                       (u1 (plist-get x :url))
                       (r1 (plist-get x :map)))
                   (when u1
                     (push! (list l1 u1) treesit-language-source-alist t))
                   (push! (cons m1 r1) major-mode-remap-alist t)
                   (dolist (y auto-mode-alist)
                     (and (eq (cdr y) m1)
                          (push! (cons (car y) r1) aa)))))
               (setq auto-mode-alist aa))))
    (when-interactive%
      (message "treesit %s" (if on "off" "on")))))

(when-version% > 30
  (defun treesit*-grammar-install (&rest args)
    (let ((dir (car treesit-extra-load-path)))
      (apply #'treesit--install-language-grammar-1
             (append (list dir) (cdr args))))))

(defun on-treesit-init! ()
  "On \\=`treesit\\=' initialization."
  ;; default load path
  (if-version%
      < 30
      (when-var% treesit--install-language-grammar-out-dir-history treesit
        (setq treesit--install-language-grammar-out-dir-history
              (list (car treesit-extra-load-path)
                    (cdr treesit--install-language-grammar-out-dir-history))))
    (fset 'treesit--install-language-grammar-1 #'treesit*-grammar-install))
  ;;; `on-c-ts-mode-init!'
  (declare-function on-c-ts-mode-init! (v-home%> "config/cc"))
  (autoload 'on-c-ts-mode-init! (v-home%> "config/cc"))
  (with-eval-after-load 'c-ts-mode (on-c-ts-mode-init!))

  ;;; `on-python-init!' for `python-ts-mode'
  (declare-function on-python-init! (v-home%> "config/pythons"))
  (autoload 'on-python-init! (v-home%> "config/pythons"))
  (with-eval-after-load 'python-ts-mode (on-python-init!)))



(provide 'treesits)

;; end of treesits.el
