;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-net-autoload.el
;;;;


(require 'net-utils)


(defun netstat* (&optional arg)
  "Run `netstat-program' and display diagnostic output."
  (interactive "snetstat options: ")
  (with-var netstat-program-options
    (setq% netstat-program-options
           (if arg
               (list arg)
             netstat-program-options)
           'net-utils)
    (netstat)))
