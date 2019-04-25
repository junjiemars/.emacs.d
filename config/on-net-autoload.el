;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-net-autoload.el
;;;;


(defun *arp (&optional arg)
  "Run `arp-program' and display diagnostic output."
  (interactive (when current-prefix-arg
                 (list (read-from-minibuffer "Arp options: "))))
  (when-fn% 'arp 'net-utils
    (require 'net-utils)
    (let ((arp-program-options (if arg (split-string* arg " " t)
                                 arp-program-options)))
      (arp))))


(defun *ifconfig (&optional arg)
  "Run `ifconfig-program' and display diagnostic output."
  (interactive (when current-prefix-arg
                 (list (read-from-minibuffer "Ifconfig options: "))))
  (when-fn% 'ifconfig 'net-utils
    (require 'net-utils)
    (let ((ifconfig-program-options (if arg (split-string* arg " " t)
                                      ifconfig-program-options)))
      (ifconfig))))


(defun *netstat (&optional arg)
  "Run `netstat-program' and display diagnostic output."
  (interactive (when current-prefix-arg
                 (list (read-from-minibuffer "Netstat options: "))))
  (when-fn% 'netstat 'net-utils
    (require 'net-utils)
    (let ((netstat-program-options (if arg (split-string* arg " " t)
                                     netstat-program-options)))
      (netstat))))


(defun *ping (&optional arg)
  "Run `ping-program' ping host."
  (interactive (when current-prefix-arg
                 (list (read-from-minibuffer "Ping options: "))))
  (when-fn% 'netstat 'net-utils
    (require 'net-utils)
    (let ((ping-program-options (if arg (split-string* arg " " t)
                                  ping-program-options)))
      (call-interactively #'ping))))

