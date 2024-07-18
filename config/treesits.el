;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; treesits.el
;;;;

(defalias 'treesit*-recipe
  (lexical-let%
      ((b (v-home% ".exec/treesit-settings.el"))
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
                   :map c-or-c++-ts-mode))))
    (lambda (&optional op sexp)
      (cond ((eq op :put)
             (dolist* (x sexp) (push! x m t)))
            ((eq op :read)
             (setq m (read-sexp-from-file b)))
            ((eq op :dump)
             (when m (save-sexp-to-file m b)))
            (t m))))
  "The \\=`treesit\\=' recipe.")

(defun toggle-treesit! ()
  "Toggle \\=`treesit\\=' on or off."
  (interactive)
  (let ((on (let ((ts (treesit*-recipe)))
              (some* (lambda (a)
                       (catch 'break
                         (dolist* (x ts)
                           (when (eq (plist-get x :map) (cdr a))
                             (throw 'break t)))))
                     major-mode-remap-alist))))
    (if on
        (let ((ts (treesit*-recipe)))
          (setq auto-mode-alist
                (remove-if* (lambda (a)
                              (catch 'break
                                (dolist* (x ts)
                                  (when (eq (plist-get x :map) a)
                                    (throw 'break t)))))
                            auto-mode-alist
                            :key #'cdr)
                major-mode-remap-alist
                (remove-if* (lambda (a)
                              (catch 'break
                                (dolist* (x ts)
                                  (when (eq (plist-get x :map) a)
                                    (throw 'break t)))))
                            major-mode-remap-alist
                            :key #'cdr)
                treesit-language-source-alist nil)
          (message "%s" "treesit off"))
      (let ((ts (treesit*-recipe))
            (aa (copy-sequence auto-mode-alist)))
        (dolist* (x ts)
          (let ((l1 (plist-get x :lang))
                (m1 (plist-get x :mode))
                (u1 (plist-get x :url))
                (r1 (plist-get x :map)))
            (when u1
              (push! (list l1 u1) treesit-language-source-alist t))
            (push! (cons m1 r1) major-mode-remap-alist t)
            (dolist* (y auto-mode-alist)
              (when (eq (cdr y) m1)
                (push! (cons (car y) r1) aa)))))
        (setq auto-mode-alist aa)
        (message "%s" "treesit on")))))

(defadvice treesit--install-language-grammar-1
    (before treesit--install-language-grammar-1-before first compile disable)
  (unless (ad-get-arg 0)
    (ad-set-arg 0 (car treesit-extra-load-path))))

(defun on-treesit-init! ()
  "On \\=`treesit\\=' initialization."
  (setq% treesit-extra-load-path `(,(v-home% ".treesit/")))
  (ad-enable-advice #'treesit--install-language-grammar-1 'before
                    "treesit--install-language-grammar-1-before")
  (ad-activate #'treesit--install-language-grammar-1 t))



(provide 'treesits)

;; end of treesits.el
