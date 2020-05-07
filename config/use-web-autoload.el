;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-web-autoload.el
;;;;


(defmacro-if-feature% js2-mode)


(if-feature-js2-mode%
    (with-eval-after-load 'js2-mode

      (when (self-spec->*env-spec :edit :allowed)
        (setq js2-basic-offset
              (self-spec->*env-spec :edit :tab-width)))))



;; end of file

