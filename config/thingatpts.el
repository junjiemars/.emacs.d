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

(defmacro unless-fn-thing-at-point-bounds-of-string-at-point% (&rest body)
  (declare (indent 0))
  (if-fn 'thing-at-point-bounds-of-string-at-point 'thingatpt
         `(comment ,@body)
    `(progn% ,@body)))

(defmacro unless-fn-thing-at-point-bounds-of-list-at-point% (&rest body)
  (declare (indent 0))
  (if-fn 'thing-at-point-bounds-of-list-at-point 'thingatpt
         `(comment ,@body)
    `(progn% ,@body)))

(unless-fn-thing-at-point-bounds-of-string-at-point%
  (defun thing-at-point-bounds-of-string-at-point ()
    "Return the bounds of the double quoted string at point."
    (save-excursion
      (let ((beg (nth 8 (syntax-ppss))))
        (when beg
          (goto-char beg)
          (forward-sexp)
          (cons (1+  beg) (1- (point))))))))

(unless-fn-thing-at-point-bounds-of-string-at-point%
  (put 'string 'bounds-of-thing-at-point
       'thing-at-point-bounds-of-string-at-point))

(unless-fn-thing-at-point-bounds-of-list-at-point%
  (defun thing-at-point-bounds-of-list-at-point ()
    "Return the bounds of the list at point."
    (save-excursion
      (let* ((st (parse-partial-sexp (point-min) (point)))
             (beg (or (and (eq 4 (car (syntax-after (point))))
                           (not (nth 8 st))
                           (point))
                      (nth 1 st))))
        (when beg
          (goto-char beg)
          (forward-sexp)
          (cons beg (point)))))))

(unless-fn-thing-at-point-bounds-of-list-at-point%
  (put 'list 'bounds-of-thing-at-point
       'thing-at-point-bounds-of-list-at-point))

(unless% (or (get 'defun 'beginning-of-defun)
             (get 'defun 'end-of-defun))
  ;; fix wrong behavior on ancient Emacs.
  (put 'defun 'beginning-op 'beginning-of-defun)
  (put 'defun 'end-op       'end-of-defun)
  (put 'defun 'forward-op   'end-of-defun))

;; end of require

(defun mark-thing (begin end)
  "Mark thing at point."
  (goto-char begin)
  (set-mark (point))
  (goto-char end))

(defun symbol@ (&optional thing)
  "Return the (cons \\='region|nil THING) at point."
  (if-region-active
      (let ((ss (buffer-substring-no-properties
                 (region-beginning) (region-end))))
        (setq mark-active nil)
        (cons 'region ss))
    (let ((ss (thing-at-point (or thing 'symbol))))
      (and ss (cons nil (substring-no-properties ss))))))



(provide 'thingatpts)

;; end of thingatpts.el
