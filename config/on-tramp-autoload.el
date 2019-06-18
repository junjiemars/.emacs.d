;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-tramp-autoload.el
;;;;


(when% (and (executable-find% "docker")
            (require 'tramp)
            (not (find "docker" tramp-methods :key #'car :test #'string=)))
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-methods
                 '("docker"
                   (tramp-login-program "docker")
                   (tramp-login-args
                    (nil
                     ("exec" "-it")
                     ("-u" "%u")
                     ("%h")
                     ("sh")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-i" "-c")))
                 nil
                 (lambda (a b)
                   (string= (car a) (car b))))))


;; end of on-tramp-autoload.el

