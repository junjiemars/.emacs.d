;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; thingatpts.el
;;;;
;; Commentary: `thingatpt' compatible definitions.
;;;;


;;; require

;;; fix wrong behavior on anicent Emacs.

(eval-when-compile
  (defmacro unless-thing-at-point-bounds-of-string-at-point% (&rest body)
    (declare (indent 0))
    `(unless-fn% thing-at-point-bounds-of-string-at-point thingatpt
       (progn% ,@body))))

(eval-when-compile
  (defmacro unless-thing-at-point-bounds-of-list-at-point% (&rest body)
    (declare (indent 0))
    `(unless-fn% thing-at-point-bounds-of-list-at-point thingatpt
       (progn% ,@body))))

(unless-thing-at-point-bounds-of-string-at-point%
  (defun thing-at-point-bounds-of-string-at-point ()
    "Return the bounds of the double quoted string at point."
    (save-excursion
      (let ((beg (nth 8 (syntax-ppss))))
        (when beg
          (goto-char beg)
          (forward-sexp)
          (cons (1+  beg) (1- (point))))))))

(unless-thing-at-point-bounds-of-list-at-point%
  (defun thing-at-point-bounds-of-list-at-point ()
    "Return the bounds of the list at point."
    (save-excursion
      (let* ((st (parse-partial-sexp (point-min) (point)))
             (beg (or (and (eq 4 (car (syntax-after (point))))
                           (null (nth 8 st))
                           (point))
                      (nth 1 st))))
        (when beg
          (goto-char beg)
          (forward-sexp)
          (cons beg (point)))))))

;; end of require

;;; fix wrong behavior on ancient Emacs.
(defun on-thingatpt-init! ()
  (unless-thing-at-point-bounds-of-string-at-point%
    (put 'string 'bounds-of-thing-at-point
         'thing-at-point-bounds-of-string-at-point))
  (unless-thing-at-point-bounds-of-list-at-point%
    (put 'list 'bounds-of-thing-at-point
         'thing-at-point-bounds-of-list-at-point))
  (unless% (eq 'beginning-of-defun (get 'defun 'beginning-op))
    (put 'defun 'beginning-op 'beginning-of-defun))
  (unless% (eq 'end-of-defun (get 'defun 'end-op))
    (put 'defun 'end-op 'end-of-defun))
  (unless% (eq 'end-of-defun (get 'defun 'forward-op))
    (put 'defun 'forward-op 'end-of-defun)))



(provide 'thingatpts)

;; end of thingatpts.el
