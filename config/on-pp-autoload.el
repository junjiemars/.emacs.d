;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-pp-autoload.el - pretty print
;;;;


;; xml

(defun pp-xml (begin end &optional arg)
  "Pretty-pprint XML region.

If ARG < 0 then minify the region, otherwise pretty print it."
  (interactive
   (list (region-beginning) (region-end)
         current-prefix-arg))
  (save-excursion
    (if (and (numberp arg) (< arg 0))
        (let ((s (replace-regexp-in-string
                  ">[ \t\n]+<" "><"
                  (delete-and-extract-region begin end))))
          (goto-char begin)
          (insert s)
          (set-mark (point)))
      (xml-mode)
      (goto-char begin)
      (while (search-forward-regexp ">[ \t]*<[^/]" end t)
        (backward-char 2) (insert "\n") (incf end))
      (goto-char begin)
      (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
        (backward-char) (insert "\n") (incf end))
      (indent-region begin end nil)
      (normal-mode))))


;; (if-fn% 'sgml-pretty-print 'sgml-mode
;;         (defalias 'pp-xml #'sgml-pretty-print
;;           "Pretty-print XML region.")
;;   )


(if-fn% 'json-pretty-print 'json
        (defalias 'pp-json #'json-pretty-print
          "Pretty-print Json region."))


;; end of on-pp-autoload.el
