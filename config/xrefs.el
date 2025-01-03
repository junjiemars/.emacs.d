;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; xrefs.el
;;;;
;; Commentary:
;;;;


;;; require

;; end of require

;;; `xref-find-definitions' associated macro, since emacs25

(eval-when-compile
  (defmacro when-fn-xref-find-definitions% (&rest body)
    (declare (indent 0))
    (if-fn% xref-find-definitions xref
            `(progn% ,@body)
      `(comment ,@body))))

(eval-when-compile
  (defmacro unless-fn-xref-find-definitions% (&rest body)
    (declare (indent 0))
    (if-fn% xref-find-definitions xref
            `(comment ,@body)
      `(progn% ,@body))))

;; end of `xref-find-definitions' associated macro

;;; `xref--show-location' associated macro, since emacs25

(eval-when-compile
  (defmacro when-fn-xref--show-location% (&rest body)
    (declare (indent 0))
    (if-fn% xref--show-location xref
            `(progn% ,@body)
      `(comment ,@body))))

;; end of `xref--show-location' associated macro

;;;
;; go into `view-mode'
;; `xref-find-definitions' into `view-mode'
;;;

(defalias 'xref*-read-only-dirs
  (let ((f (v-home% ".exec/xref-read-only-dirs.el"))
        (b `(,(when% (> (path-depth invocation-directory) 1)
                (path- invocation-directory))
             ,(when-package% package*-user-dir))))
    (lambda (&optional op dir)
      (cond ((eq op :read) (setq b (read-sexp-from-file f)))
            ((eq op :push) (when (> (length dir) 0)
                             (push! dir b t)))
            ((eq op :save) (save-sexp-to-file b f))
            ((eq op :file) f)
            (t b)))))

(defun xref*-buffer-in-view-mode (&optional buffer)
  (let* ((buf (or buffer (current-buffer)))
         (name (buffer-file-name buf)))
    (when (and name (file-in-dirs-p name (xref*-read-only-dirs)))
      (with-current-buffer buf
        (view-mode 1)))))

(when-fn-xref-find-definitions%
  ;; `xref-find-definitions' into `view-mode'
  (defadvice xref-find-definitions
      (after xref-find-definitions-after first compile disable)
    (xref*-buffer-in-view-mode)))

(when-fn-xref--show-location%
  ;; `xref--show-location' into `view-mode'
  (defadvice xref--show-location
      (after xref--show-location-after first compile disable)
    (xref*-buffer-in-view-mode (window-buffer ad-return-value))))

(defun on-xref-init! ()
  (when (file-exists-p (xref*-read-only-dirs :file))
    (xref*-read-only-dirs :read))
  (when-fn-xref-find-definitions%
    (ad-enable-advice #'xref-find-definitions 'after
                      "xref-find-definitions-after")
    (ad-activate #'xref-find-definitions t))
  (when-fn-xref--show-location%
    (ad-enable-advice #'xref--show-location 'after
                      "xref--show-location-after")
    (ad-activate #'xref--show-location t))
  (unless-graphic%
    (when% (facep 'xref-match)
      (set-face-background 'xref-match +term-background-color+)
      (set-face-foreground 'xref-match +term-foreground-color+))))


(unless-fn% xref-find-references xref
  (defun xref-find-references (what)
    "Alias of \\=`tags-apropos\\='."
    (interactive
     (list (read-string "Find references of: " (cdr (symbol@ 'symbol)))))
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
    (xref*-buffer-in-view-mode)))

(defun on-etags-init! ()
  "On \\=`etags\\=' initialization."
  (when-fn-pop-tag-mark%
   ;; define keys for `pop-tag-mark' and `tags-loop-continue'
   (define-global-key% (kbd "M-,") #'pop-tag-mark)
   (define-global-key% (kbd "M-*") #'tags-loop-continue))
  (unless-fn-xref-find-definitions%
    (ad-enable-advice #'find-tag 'after "find-tag-after")
    (ad-activate #'find-tag t)))

;; end of `etags'


(provide 'xrefs)


;; end of xrefs.el
