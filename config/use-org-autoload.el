;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-org-autoload.el (Lisp Flavoured Erlang)
;;;;


(defmacro-if-feature% ox-reveal)

(with-eval-after-load 'org

  ;; load `ox-reveal' if it had been installed.
  (if-feature-ox-reveal%
    (require 'ox-reveal)))



;; EOF
