;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-net-autoload.el
;;;;


(when-fn% 'arp 'net-utils
  (defun *arp (&optional arg)
    "Run `arp-program' and display diagnostic output."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Arp options: "))))
    (require 'net-utils)
    (let ((arp-program-options (if arg (split-string* arg " " t)
                                 arp-program-options)))
      (arp))))


(when-fn% 'ifconfig 'net-utils
  (defun *ifconfig (&optional arg)
    "Run `ifconfig-program' and display diagnostic output."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Ifconfig options: "))))
    (require 'net-utils)
    (if-var% ifconfig-program-options 'net-utils
             (let ((ifconfig-program-options (if arg (split-string* arg " " t)
                                               ifconfig-program-options)))
               (ifconfig))
      (let ((ipconfig-program-options (if arg (split-string* arg " " t)
                                        ipconfig-program-options)))
        (ipconfig)))))


(when-fn% 'netstat 'net-utils
  (defun *netstat (&optional arg)
    "Run `netstat-program' and display diagnostic output."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Netstat options: "))))
    (require 'net-utils)
    (let ((netstat-program-options (if arg (split-string* arg " " t)
                                     netstat-program-options)))
      (netstat))))


(when-fn% 'netstat 'net-utils
  (defun *ping (&optional arg)
    "Run `ping-program' ping host."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Ping options: "))))
    (require 'net-utils)
    (let ((ping-program-options (if arg (split-string* arg " " t)
                                  ping-program-options)))
      (call-interactively #'ping t))))

