;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; pp.el - pretty print
;;;;

(if-fn% 'sgml-pretty-print 'sgml-mode
        (defalias 'pp-xml #'sgml-pretty-print
          "Pretty-print XML region.")
  (when-fn% 'nxml-mode 'nxml-mode
    (defun pp-xml (begin end)
      "Pretty-pprint XML region."
      (interactive "r")
      (save-excursion
        (nxml-mode)
        (goto-char begin)
        ;; split <foo><foo> or </foo><foo>, but not <foo></foo>
        (while (search-forward-regexp ">[ \t]*<[^/]" end t)
          (backward-char 2) (insert "\n") (incf end))
        ;; split <foo/></foo> and </foo></foo>
        (goto-char begin)
        (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
          (backward-char) (insert "\n") (incf end))
        (indent-region begin end nil)
        (normal-mode)))))


(if-fn% 'json-pretty-print 'json
        (defalias 'pp-json #'json-pretty-print
          "Pretty-print Json region."))


;; end of pp.el
