;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-treesit-autoload.el
;;;;


(defmacro-if-feature% treesit)


(defmacro when-feature-treesit% (&rest body)
  "When \\=`treesit\\=', do BODY."
  (if-feature-treesit%
      (if (treesit-available-p)
          `(progn% ,@body)
        `(comment ,@body))))

;; end of macro


(when-feature-treesit%

 (defalias 'treesit*-settings
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
       (cond ((eq op :push)
              (dolist* (x sexp) (push! x m t)))
             ((eq op :read)
              (setq m (read-sexp-from-file b)))
             ((eq op :dump)
              (when m (save-sexp-to-file m b)))
             (t m))))
   "The \\=`treesit\\=' settings."))


(when-feature-treesit%

 (defun toggle-treesit! ()
   "Toggle \\=`treesit\\=' on or off."
   (interactive)
   (let ((on (let ((ts (treesit*-settings)))
               (some* (lambda (a)
                        (catch 'break
                          (dolist* (x ts)
                            (when (eq (plist-get x :map) (cdr a))
                              (throw 'break t)))))
                      major-mode-remap-alist))))
     (if on
         (let ((ts (treesit*-settings)))
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
           (message "treesit off"))
       (let ((ts (treesit*-settings))
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
         (message "treesit on"))))))


(when-feature-treesit%
 (defadvice treesit--install-language-grammar-1
     (before treesit--install-language-grammar-1-before compile)
   (unless (ad-get-arg 0)
     (ad-set-arg 0 (car treesit-extra-load-path)))))


(when-feature-treesit%
 (with-eval-after-load 'treesit
   (setq% treesit-extra-load-path `(,(v-home% ".treesit/")))
   (ad-enable-advice #'treesit--install-language-grammar-1 'before
                     "treesit--install-language-grammar-1-before")
   (ad-activate #'treesit--install-language-grammar-1 t)))


;; end of on-treesit-autoload.el
