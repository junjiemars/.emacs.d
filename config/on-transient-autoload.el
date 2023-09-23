;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-transient-autoload.el
;;;;


(defmacro-if-feature% transient)


(defmacro when-feature-transient% (&rest body)
  "When \\=`transient\\=', do BODY."
  (if-feature-transient%
      `(progn% ,@body)))


(when-feature-transient%
    (with-eval-after-load 'transient

      (let ((dir (v-home! ".transient/")))
        (setq transient-history-file (concat dir "history.el")
              transient-levels-file (concat dir "levels.el")
              transient-values-file (concat dir "values.el")))))


(unless-graphic%
  ;; above version 23 transient-mark-mode is enabled by default
  (when-version% > 23 (transient-mark-mode t))
  ;; fix some terminal theme confused with background and foreground.
  (set-face-background 'region "white")
  (set-face-foreground 'region "black"))


 ; end of on-transient-autoload.el