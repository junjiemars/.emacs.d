;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eww-autoload.el
;;;;

(declare-function on-eww-init! (v-home%> "config/ewws.el"))
(declare-function lookup-web (v-home%> "config/ewws.el"))

;;; `eww' after load
(with-eval-after-load 'eww
  (on-eww-init!))

;;; `eww-search-words' and `webjump' more leaner than `lookup-web'.
(define-key% (current-global-map) (kbd "M-s w") #'lookup-web)
(when-fn% 'eww-list-bookmarks 'eww
  (define-key% (current-global-map) (kbd "M-s M-b")
               #'eww-list-bookmarks))


;; end of on-eww-autoload.el
