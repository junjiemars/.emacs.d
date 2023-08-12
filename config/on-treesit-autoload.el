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
   "Toggle \\=`treesit\\='."
   (interactive)
   (if major-mode-remap-alist
       (let ((ms major-mode-remap-alist))
         (while (car ms)
           (setq auto-mode-alist
                 (remove-if* (lambda (x) (eq (cdar ms) x))
                             auto-mode-alist
                             :key #'cdr))
           (setq ms (cdr ms)))
         (setq major-mode-remap-alist nil
               treesit-language-source-alist nil)
         (message "treesit off"))
     (let ((ss (treesit*-settings)) (rr nil) (mm nil)
           (aa (copy-sequence auto-mode-alist)))
       (dolist* (x ss)
         (let ((l1 (plist-get x :lang))
               (m1 (plist-get x :mode))
               (u1 (plist-get x :url))
               (r1 (plist-get x :map)))
           (when u1 (push! (list l1 u1) rr t))
           (push! (cons m1 r1) mm t)
           (dolist* (y auto-mode-alist)
             (when (eq (cdr y) m1)
               (push! (cons (car y) r1) aa)))))
       (setq treesit-language-source-alist rr
             major-mode-remap-alist mm
             auto-mode-alist aa)
       (message "treesit on")))))


(when-feature-treesit%

 (with-eval-after-load 'treesit

   (setq% treesit-extra-load-path
          `(,(v-home! ".treesit/"))
          'treesit)))


 ; end of `treesit'


;;; end of on-treesit-autoload.el
