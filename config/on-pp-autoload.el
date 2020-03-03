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

(defun pp-json (begin end &optional arg)
  "Pretty-print Json region.

If ARG < 0 then minify the region, otherwise pretty print it."
  (interactive (list (region-beginning) (region-end)
                     current-prefix-arg))
  (if (and (numberp arg) (< arg 0))
      ;; TODO: {"a":  "aa\" \naaa" }
      (insert
       (replace-regexp-in-string "\\(\"\\(?:[^\"\\]|.\\)*\"\\)\\|\s+"
                                 ""
                                 (delete-and-extract-region begin end)))
    (if-fn% 'json-pretty-print 'json
            (json-pretty-print begin end)
      (message "!pp-json had not been implemented"))))



;; end of on-pp-autoload.el
