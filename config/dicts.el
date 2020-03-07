;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; dicts.el
;;;;


(eval-when-compile
  (require 'url)
  (require 'url-util))


(defvar *dicts*
  '(("bing" (:url "http://www.bing.com/dict/search?mkt=zh-cn&q="
                  :pronounce ""
                  :area (""))))
  "Dictionaries using by `lookup-dict'.")



(defun on-lookup-dict (status keyword)
  "Callback when `lookup-dict'."
  (set-buffer-multibyte t)
  (write-region (point-min) (point-max) (emacs-home* "private/dict.txt"))
  )


(defun lookup-dict (word &optional dict)
  "Show the explanation of WORD from DICT in the echo area."
  (interactive
   (list (read-string "lookup dict for: " (cdr (symbol@)))
         (when current-prefix-arg
           (read-string (format "Choose (%s): "
                                (mapconcat
                                 #'identity
                                 (mapcar #'car *dicts*)
                                 "|"))))))
  (let* ((d1 (if (or (null dict)
                     (string= "" dict))
                 "bing"
               dict))
         (d2 (plist-get (cadr (assoc** d1 *dicts* #'string=)) :url)))
    (url-retrieve (concat d2 (url-hexify-string word))
                  #'on-lookup-dict
                  `(,(decode-coding-string word 'utf-8))
                  t
                  t)))


(define-key (current-global-map) (kbd "C-c d") #'lookup-dict)

;; end of dicts.el
