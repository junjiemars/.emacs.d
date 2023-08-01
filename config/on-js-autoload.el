;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-js-autoload.el
;;;;



(with-eval-after-load 'js

  (when-version% > 28.1
    (setq% js-js-tmpdir (v-home! ".js/") 'js))

  (when-var% js-indent-level 'js
    (when (*self-env-spec* :get :edit :allowed)
      (setq% js-indent-level
             (*self-env-spec* :get :edit :tab-width)
             'js))))


;;; eof
