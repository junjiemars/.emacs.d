;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-net-autoload.el
;;;;


(defun *ifconfig (&optional options)
  "Run `ifconfig-program' and display diagnostic output."
  (interactive (when current-prefix-arg
                 (list (read-from-minibuffer "Ifconfig options: "))))
  (when-fn% 'ifconfig 'net-utils
    (require 'net-utils)
    (let ((ifconfig-program-options (if options (list options)
                                      ifconfig-program-options)))
      (ifconfig))))


(defun *netstat (&optional options)
  "Run `netstat-program' and display diagnostic output."
  (interactive (when current-prefix-arg
                 (list (read-from-minibuffer "Netstat options: "))))
  (when-fn% 'netstat 'net-utils
    (require 'net-utils)
    (let ((netstat-program-options (if options (list options)
                                     netstat-program-options)))
      (netstat))))

