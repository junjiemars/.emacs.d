;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; treesits.el
;;;;

;;; require

(require 'treesit)

;; end of require

(defalias 'treesit*-recipe
  (let ((b (emacs-home% "config/treesit-recipe.el"))
        (file (v-home% ".exec/treesit-recipe.el"))
        (env nil))
    (lambda (&optional op n)
      (cond ((and op (eq op :file)) file)
            ((and op (eq op :scratch)) b)
            ((and op (eq op :save)) (and env (write-file* env file)))
            ((and n op (eq op :load)) (setq env n))
            (t env))))
  "The \\=`treesit\\=' recipe.")

(defun treesit*-on? ()
  "Return t if \\=`treesit\\=' is on, otherwise nil."
  (catch :br
    (dolist (x (treesit*-recipe) nil)
      (let ((m (plist-get x :map)))
        (when (or (rassq m major-mode-remap-alist)
                  (rassq m auto-mode-alist))
          (throw :br t))))))

(defun treesit*--auto-mode-remap (recipe lhs rhs)
  (dolist (a auto-mode-alist)
    (dolist (x recipe)
      (when (eq (cdr a) (plist-get x lhs))
        (setcdr a (plist-get x rhs))))))

(defun treesit*--major-mode-remap (recipe lhs rhs)
  (setq% major-mode-remap-defaults nil)
  (dolist (x recipe)
    (let* ((lhv (plist-get x lhs))
           (rhv (plist-get x rhs))
           (r1 (assq lhv major-mode-remap-alist))
           (r2 (assq rhv major-mode-remap-alist)))
      (cond (r1 (setcdr r1 rhv))
            (r2 (setcar r2 lhv) (setcdr r2 rhv))
            (t (push! (cons lhv rhv) major-mode-remap-alist))))))

(when-version% > 30
  (defun treesit--install-language-grammar-1* (&rest args)
    (let ((dir (car treesit-extra-load-path)))
      (apply '_treesit--install-language-grammar-1_
             (append (list dir) (cdr args))))))

(defun treesit*--recipe-init! ()
  "On \\=`treesit\\=' initialization."
  ;; `on-c-ts-mode-init!'
  (declare-function on-c-ts-mode-init! (v-home%> "config/cc"))
  (autoload 'on-c-ts-mode-init! (v-home%> "config/cc"))
  (with-eval-after-load 'c-ts-mode (on-c-ts-mode-init!))
  ;; `on-python-init!' for `python-ts-mode'
  (declare-function on-python-init! (v-home%> "config/pythons"))
  (autoload 'on-python-init! (v-home%> "config/pythons"))
  (with-eval-after-load 'python-ts-mode (on-python-init!))
  ;; default load path
  (if-version%
      <= 30
      (when-var% treesit--install-language-grammar-out-dir-history treesit
        (push! (car treesit-extra-load-path)
               treesit--install-language-grammar-out-dir-history))
    (defadvice* '_treesit--install-language-grammar-1_
      'treesit--install-language-grammar-1
      #'treesit--install-language-grammar-1*))
  ;; load recipe
  (let ((recipe (treesit*-recipe :file)))
    (unless (file-exists-p recipe)
      (copy-file (treesit*-recipe :scratch) recipe t))
    (treesit*-recipe :load (read-file* recipe t))))

(defun toggle-treesit! ()
  "Toggle \\=`treesit\\=' on or off."
  (interactive)
  (let ((ts (or (treesit*-recipe) (treesit*--recipe-init!)))
        (on (treesit*-on?)))
    (cond (on (treesit*--auto-mode-remap ts :map :mode)
              (treesit*--major-mode-remap ts :map :mode))
          (t (treesit*--auto-mode-remap ts :mode :map)
             (treesit*--major-mode-remap ts :mode :map)))
    (when-interactive%
      (message "treesit %s" (if (treesit*-on?) "enabled" "disabled")))))



(provide 'treesits)

;; end of treesits.el
