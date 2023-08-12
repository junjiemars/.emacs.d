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
  (when (treesit-available-p)
    (if-feature-treesit%
        `(progn% ,@body))))


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
              (dolist* (x sexp)
                (push! x m t)))
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
   (let ((on (catch 'rc
               (let ((ss (treesit*-settings)))
                 (dolist* (x ss)
                   (when (some* (lambda (a)
                                  (eq (plist-get x :map) (cdr a)))
                                major-mode-remap-alist)
                     (throw 'rc t)))))))
     (if on
         (let ((ts (treesit*-settings)))
           (dolist* (x ts)
             (setq auto-mode-alist
                   (remove-if* (lambda (a) (eq (plist-get x :map) a))
                               auto-mode-alist
                               :key #'cdr)
                   major-mode-remap-alist
                   (remove-if* (lambda (a) (eq (plist-get x :map) a))
                               major-mode-remap-alist
                               :key #'cdr)))
           (setq treesit-language-source-alist nil)
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

 (with-eval-after-load 'treesit

   (setq% treesit-extra-load-path
          `(,(v-home! ".treesit/"))
          'treesit)))


 ; end of `treesit'


;;; end of on-treesit-autoload.el
