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
                  :us-pronounce
                  "<div class=\"hd_prUS b_primtxt\">\\(.+\\)</div>"
                  :area (""))))
  "Dictionaries using by `lookup-dict'.")



(defun on-lookup-dict (status &rest args)
  "Callback when `lookup-dict'."
  (declare (indent 1))
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
         (d2 (cadr (assoc** d1 *dicts* #'string=)))
         (url (plist-get d2 :url)))
    (url-retrieve (concat url (url-hexify-string word))
                  #'on-lookup-dict
                  `(:word ,(decode-coding-string word 'utf-8)
                          :dict ,d2
                          :style nil)
                  t
                  t)))


(define-key (current-global-map) (kbd "C-c d") #'lookup-dict)

;; end of dicts.el
