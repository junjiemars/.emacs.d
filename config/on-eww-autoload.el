;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eww-autoload.el
;;;;

(declare-function on-eww-init! (v-home%> "config/ewws"))
(declare-function lookup-web (v-home%> "config/ewws"))
(autoload 'on-eww-init! (v-home%> "config/ewws"))

;;; `eww' after load
(with-eval-after-load 'eww
  (on-eww-init!))

;;; autoload
(autoload 'lookup-web (v-home%> "config/ewws")
  "Lookup web." t)
(autoload 'toggle-browser! (v-home%> "config/ewws")
  "Toggle browser." t)

;;; `eww-search-words' and `webjump' more leaner than `lookup-web'.
(define-key% (current-global-map) (kbd "M-s w") #'lookup-web)
(when-fn% 'eww-list-bookmarks 'eww
  (define-key% (current-global-map) (kbd "M-s M-b")
               #'eww-list-bookmarks))

;; end of on-eww-autoload.el
