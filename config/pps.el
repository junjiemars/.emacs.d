;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; pps.el
;;;;
;; Commentary: pretty print
;;;;


(when-version% >= 25
  (declare-function forward-whitespace "subr"))

;; elisp

(defmacro pprint (form)
  "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
  `(cl-prettyprint ,form))
(autoload 'cl-prettyprint "cl-extra")


;;; xml

(defun pp*-xml (begin end &optional arg)
  "Pretty pprint XML region."
  (interactive (list (region-beginning) (region-end)
                     current-prefix-arg))
  (save-excursion
    (if (and (numberp arg) (< arg 0))
        (let ((s (replace-regexp-in-string
                  ">[ \t\n]+<" "><"
                  (delete-and-extract-region begin end))))
          (goto-char begin)
          (insert s)
          (set-mark (point)))
      (with-current-buffer (current-buffer)
        (eval-when-compile (require 'sgml-mode))
        (progn
          (declare-function sgml-pretty-print "sgml-mode")
          (require 'sgml-mode)
          (sgml-pretty-print begin end))))))

;; end of xml

;;; json

(defun pp*-json (&optional minify)
  "Pretty print Json region or current buffer."
  (interactive "P")
  (let ((begin (if-region-active (region-beginning) (point-min)))
        (end (if-region-active (region-end) (point-max))))
    (if minify
        (make-thread*
         (lambda ()
           (narrow-to-region begin end)
           (goto-char (point-min))
           (while (forward-whitespace 1)
             (unless (bounds-of-thing-at-point 'string)
               (let ((bw (bounds-of-thing-at-point 'whitespace)))
                 (delete-whitespace-rectangle (car bw)
                                              (cdr bw)))))
           (goto-char (point-min))
           (while (and (forward-line 1)
                       (< (point) (point-max)))
             (delete-char -1 t))))
      (if-fn% 'json-pretty-print 'json
              (make-thread* (lambda ()
                              (json-pretty-print begin end)))
        (message (propertize "No implemented"
                             'face 'font-lock-warning-face))))))

;; end of json

(provide 'pps)

;; end of pps.el
