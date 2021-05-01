;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-web-autoload.el
;;;;


(defmacro-if-feature% js2-mode)


(if-feature-js2-mode%
    (with-eval-after-load 'js2-mode

      (when (*self-env-spec* :get :edit :allowed)
        (setq js2-basic-offset
              (*self-env-spec* :get :edit :tab-width)))))



;; end of file

