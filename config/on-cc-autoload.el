;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-cc-autoload.el
;;;;
;; Commentary: `cc' autoload.
;;;;

(declare-function on-cc-mode-init! (v-home%> "config/cc.el"))
(declare-function on-cmacexp-init! (v-home%> "config/cc.el"))
(declare-function on-man-init! (v-home%> "config/cc.el"))
(autoload 'on-cc-mode-init! (v-home%> "config/cc.el"))
(autoload 'on-cmacexp-init! (v-home%> "config/cc.el"))
(autoload 'on-man-init! (v-home%> "config/cc.el"))


;; default `c-mode-hook'
 ;; involving useless `macrostep-c-mode-hook'.
(setq% c-mode-hook nil 'cc-mode)

;;; `cc-mode' after load
(with-eval-after-load 'cc-mode
  (on-cc-mode-init!))

;;; `cmacexp' after load

(defmacro-if-feature% cmacexp)

(if-feature-cmacexp%
 (with-eval-after-load 'cmacexp
   (on-cmacexp-init!)))

;;; `man' after load
(with-eval-after-load 'man
  (on-man-init!))

;;; autoload
(autoload 'cc*-compiler (v-home%> "config/cc.el"))
(autoload 'cc*-system-include (v-home%> "config/cc.el"))
(autoload 'cc*-make-system-tags (v-home%> "config/cc.el"))


;; end of on-cc-autoload.el
