;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-treesit-autoload.el
;;;;


(defmacro-if-feature% treesit)

(defmacro when-feature-treesit% (&rest body)
  "When \\=`treesit'\\=, do BODY."
  (when (treesit-available-p)
    (if-feature-treesit%
        `(progn% ,@body))))


(when-feature-treesit%

 (with-eval-after-load 'treesit
   (setq% treesit-extra-load-path `(,(v-home! ".treesit/")) 'treesit)))





;;; end of on-treesit-autoload.el
