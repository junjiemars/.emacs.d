;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-xref-autoload.el
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

(when-fn-xref-find-definitions%
  ;; `xref-find-definitions' into `view-mode'
  (defadvice xref-find-definitions
      (after xref-find-definitions-after disable)
    (with-current-buffer (current-buffer)
      (when (file-in-dirs-p (buffer-file-name (current-buffer))
                            (tags-in-view-mode))
        (view-mode 1)))))

(when-fn-xref-find-definitions%
  (defun on-xref-init! ()
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

(defmacro when-feature-etags% (&rest body)
  `(unless-fn-xref-find-definitions% ,@body))

(defmacro when-fn-pop-tag-mark% (&rest body)
  `(unless-fn-xref-find-definitions% ,@body))

;;; `find-tag' into `view-mode'
(unless-fn-xref-find-definitions%
  (defadvice find-tag (after find-tag-after disable)
    (with-current-buffer (current-buffer)
      (when (file-in-dirs-p (buffer-file-name (current-buffer))
                            (tags-in-view-mode))
        (view-mode 1)))))

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

;;;
;; after-load
;;;

;;; `xref' after load
(when-fn-xref-find-definitions%
  (with-eval-after-load 'xref
    #'on-xref-init!))

;;; `etags' after load
(when-feature-etags%
 (with-eval-after-load 'etags
   #'on-etags-init!))



;; end of on-xref-autoload.el
