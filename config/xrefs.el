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

;;; `xref-find-definitions' since emacs-25+

(eval-when-compile
  (defmacro when-xref-find-definitions% (&rest body)
    (declare (indent 0))
    (if-fn% xref-find-definitions xref
            `(progn% ,@body)
      `(comment ,@body))))

(eval-when-compile
  (defmacro unless-xref-find-definitions% (&rest body)
    (declare (indent 0))
    (if-fn% xref-find-definitions xref
            `(comment ,@body)
      `(progn% ,@body))))

;;; `xref--show-location' associated macro, since emacs-25+

(eval-when-compile
  (defmacro when-xref--show-location% (&rest body)
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
      (cond ((and op (eq op :read)) (setq b (read-sexp-from-file f)))
            ((and op (eq op :push)) (and (> (length dir) 0)
                                         (push! dir b delete)))
            ((and op (eq op :save)) (save-sexp-to-file b f))
            ((and op (eq op :file)) f)
            (t b)))))

(defun xref*-buffer-in-view-mode (&optional buffer)
  (let* ((buf (or buffer (current-buffer)))
         (name (buffer-file-name buf)))
    (when (and name (file-in-dirs-p name (xref*-read-only-dirs)))
      (with-current-buffer buf
        (view-mode 1)))))

(when-xref-find-definitions%
  (defun xref-find-definitions* (&rest _)
    "Into \\=`view-mode\\=' after call \\=`xref-find-definitions\\='."
    (interactive)
    (let ((r (call-interactively '_xref-find-definitions_)))
      (prog1 r
	(xref*-buffer-in-view-mode)))))

(when-xref--show-location%
  (defun xref--show-location* (&rest args)
    "Into \\=`view-mode\\=' after call \\=`xref--show-location\\='."
    (let ((r (apply '_xref--show-location_ args)))
      (prog1 r
	(xref*-buffer-in-view-mode (window-buffer r))))))

(defun on-xref-init! ()
  (when (file-exists-p (xref*-read-only-dirs :file))
    (xref*-read-only-dirs :read))
  (when-xref-find-definitions%
    (defadvice* '_xref-find-definitions_
      'xref-find-definitions #'xref-find-definitions*))
  (when-xref--show-location%
    (defadvice* '_xref--show-location_
      'xref--show-location #'xref--show-location*))
  (unless-graphic%
    (when% (facep 'xref-match)
      (set-face-background 'xref-match +tui-background-color+)
      (set-face-foreground 'xref-match +tui-foreground-color+))))


(unless-fn% xref-find-references xref
  (defun xref-find-references (what)
    "Alias of \\=`tags-apropos\\='."
    (interactive
     (list (read-string "Find references of: " (symbol@* 'symbol))))
    (tags-apropos what)))


;; end of `xref'

;;;
;; `etags' after load
;; `pop-tag-mark' same as Emacs22+ for ancient Emacs
;;;

(unless-xref-find-definitions%
  (defun find-tag* (&rest _)
    "Into \\=`view-mode\\=' after call \\=`find-tag\\='."
    (interactive)
    (let ((r (call-interactively '_find-tag_)))
      (prog1 r
        (xref*-buffer-in-view-mode)))))

(defun on-etags-init! ()
  "On \\=`etags\\=' initialization."
  (unless-xref-find-definitions%
    ;; define keys for `pop-tag-mark' and `tags-loop-continue'
    (define-global-key% (kbd "M-,") #'pop-tag-mark)
    (define-global-key% (kbd "M-*") #'tags-loop-continue)
    (defadvice* '_find-tag_ 'find-tag #'find-tag*)))

;; end of `etags'


(provide 'xrefs)


;; end of xrefs.el
