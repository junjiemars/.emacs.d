;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-cc-autoload.el
;;;;
;; Commentary: `cc' autoload.
;;;;

(declare-function on-cc-mode-init! (v-home%> "config/cc"))
(declare-function on-cmacexp-init! (v-home%> "config/cc"))
(declare-function on-man-init! (v-home%> "config/cc"))
(autoload 'on-cc-mode-init! (v-home%> "config/cc"))
(autoload 'on-cmacexp-init! (v-home%> "config/cc"))
(autoload 'on-man-init! (v-home%> "config/cc"))


;; default `c-mode-hook'
 ;; involving useless `macrostep-c-mode-hook'.
(setq% c-mode-hook nil 'cc-mode)

;;; `cc-mode' after load
(with-eval-after-load 'cc-mode
  (make-thread*
   (lambda ()
     (inhibit-gc
       (on-cc-mode-init!)))))

;;; `cmacexp' after load

(defmacro-if-feature% cmacexp)

(if-feature-cmacexp%
 (with-eval-after-load 'cmacexp
   (make-thread* #'on-cmacexp-init!)))

;;; `man' after load
(with-eval-after-load 'man
  (make-thread* #'on-man-init!))

;;; autoload
(autoload 'cc*-cc (v-home%> "config/cc"))
(autoload 'cc*-system-include (v-home%> "config/cc"))
(autoload 'cc*-make-tags (v-home%> "config/cc")
  "Make system C tags." t)


;; end of on-cc-autoload.el
