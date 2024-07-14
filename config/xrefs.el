;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; xrefs.el
;;;;
;; Commentary:
;;;;

;;; `xref-find-definitions' macro

(defmacro-if-fn% xref-find-definitions xref)

(defmacro when-fn-xref-find-definitions% (&rest body)
  (declare (indent 0))
  (if-fn-xref-find-definitions%
      `(progn% ,@body)
    `(comment ,@body)))

(defmacro unless-fn-xref-find-definitions% (&rest body)
  (declare (indent 0))
  (if-fn-xref-find-definitions%
      `(comment ,@body)
    `(progn% ,@body)))

;; end of `xref-find-definitions' macro

;;;
;; go into `view-mode'
;; `xref-find-definitions' into `view-mode'
;;;

(defalias 'xref*-read-only-dirs
  (lexical-let% ((b (v-home% ".exec/xref-read-only-dirs.el"))
                 (c `(,(when% (> (path-depth invocation-directory) 1)
                         (path- invocation-directory))
                      ,(when-package% package*-user-dir))))
    (lambda (&optional op sexp)
      (cond ((eq op :read)
             (setq c (read-sexp-from-file b)))
            ((eq op :push)
             (when (> (length sexp) 0) (push! sexp c t)))
            ((eq op :save)
             (save-sexp-to-file (or sexp c) b))
            ((eq op :path) b)
            (t c)))))

(defun xref*-find-definitions ()
  (with-current-buffer (current-buffer)
    (when (file-in-dirs-p (buffer-file-name (current-buffer))
                          (xref*-read-only-dirs))
      (view-mode 1))))

(when-fn-xref-find-definitions%
  ;; `xref-find-definitions' into `view-mode'
  (defadvice xref-find-definitions
      (after xref-find-definitions-after first compile disable)
    (xref*-find-definitions)))

(defun on-xref-init! ()
  (when (file-exists-p (xref*-read-only-dirs :path))
    (xref*-read-only-dirs :read))
  (when-fn-xref-find-definitions%
    (ad-enable-advice #'xref-find-definitions 'after
                      "xref-find-definitions-after")
    (ad-activate #'xref-find-definitions t)))


(unless-fn% 'xref-find-references 'xref
  (defun xref-find-references (what)
    "Alias of \\=`tags-apropos\\='."
    (interactive
     (list (read-string "Find references of: "
                        (cdr (symbol@ 'symbol)))))
    (tags-apropos what)))


;; end of `xref'

;;;
;; `etags' after load
;; `pop-tag-mark' same as Emacs22+ for ancient Emacs
;;;

(defmacro when-fn-pop-tag-mark% (&rest body)
  `(unless-fn-xref-find-definitions% ,@body))

;;; `find-tag' into `view-mode'
(unless-fn-xref-find-definitions%
  (defadvice find-tag (after find-tag-after first compile disable)
    (xref*-find-definitions)))

(defun on-etags-init! ()
  "On \\=`etags\\=' initialization."
  (when-fn-pop-tag-mark%
   ;; define keys for `pop-tag-mark' and `tags-loop-continue'
   (define-key% (current-global-map) (kbd "M-,") #'pop-tag-mark)
   (define-key% (current-global-map) (kbd "M-*") #'tags-loop-continue))
  (unless-fn-xref-find-definitions%
    (ad-enable-advice #'find-tag 'after "find-tag-after")
    (ad-activate #'find-tag t)))

;; end of `etags'


(provide 'xrefs)


;; end of xrefs.el
