;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-pp-autoload.el - pretty print
;;;;


;; elisp

(defmacro pprint (form)
  "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
  `(cl-prettyprint ,form))
(autoload 'cl-prettyprint "cl-extra")


;; xml

(defun pp-xml (begin end &optional arg)
  "Pretty-pprint XML region.

If ARG < 0 then minify the region, otherwise pretty print it."
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
        (with-no-warnings
          (progn
            (require 'sgml-mode)
            (sgml-pretty-print begin end)))))))

(defalias 'pp-html #'pp-xml)


;; json

(defun pp-json (&optional arg)
  "Pretty-print Json region or current buffer.

Minify the region when ARG is non-nil, otherwise pretty print it."
  (interactive "P")
  (let ((begin (region-active-if (region-beginning) (point-min)))
        (end (region-active-if (region-end) (point-max))))
    (if arg
        (make-thread*
         (let ((ss (buffer-substring-no-properties begin end))
               (s1 nil))
           (with-temp-buffer
             (insert ss)
             (javascript-mode)
             (goto-char (point-min))
             (while (forward-whitespace 1)
               (when (search-backward-regexp "\\(\s+\\)" nil t)
                 (replace-match "")))
             (goto-char (point-min))
             (while (and (forward-line 1)
                         (< (point) (point-max)))
               (delete-char -1 t))
             (setq s1 (buffer-substring-no-properties (point-min)
                                                      (point-max))))
           (delete-and-extract-region begin end)
           (insert s1)))
      (if-fn% 'json-pretty-print 'json
              (make-thread* (json-pretty-print begin end))
        (message (propertize "No implemented"
                             'face 'font-lock-warning-face))))))



;; end of on-pp-autoload.el
