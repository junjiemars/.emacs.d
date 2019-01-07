;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-indent-autoload.el
;;;;



(when (self-spec->*env-spec :edit :allowed)
  (with-eval-after-load 'sh-script
    (setq% sh-basic-offset (self-spec->*env-spec :edit :tab-width) 'sh-script)))

