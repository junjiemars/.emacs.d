;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-net-autoload.el
;;;;
;; supports [C-u] prefix to set program options
;;;;


(defmacro revert-buffer-with-coding-system* (fn buffer)
  "Revert the BUFFER with `locale-coding-system' after FN had been called.

Reversion only occurred when `default-file-name-coding-system'
not equals `locale-coding-system'.
See also: `revert-buffer-with-coding-system',`set-buffer-process-coding-system'."
  `(progn
     ,fn
     (unless% (eq default-file-name-coding-system locale-coding-system)
       (when ,buffer
         (with-current-buffer ,buffer
           (revert-buffer-with-coding-system locale-coding-system t))))))


(when-fn% 'arp 'net-utils
  (defun *arp (&optional arg)
    "Run `arp-program' and display diagnostic output."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Arp options: "))))
    (require 'net-utils)
    (let ((arp-program-options (if arg (split-string* arg " " t)
                                 arp-program-options)))
      (revert-buffer-with-coding-system* (arp) "*arp*"))))


(when% (executable-find% "dig")
  ;; on Windows there are no built-in dig program you can download
  ;; BIND from http://www.isc.org/downloads/ then extract the zip into
  ;; `exec-path'.
  (when-fn% 'run-dig 'net-utils
    (if-var% dig-program-options 'net-utils
             (defun *dig (&optional arg)
               "Run `dig-program' for host."
               (interactive (when current-prefix-arg
                              (list (read-from-minibuffer "Dig options: "))))
               (require 'net-utils)
               (let ((dig-program-options (if arg (split-string* arg " " t)
                                            dig-program-options))
                     (current-prefix-arg (when (numberp current-prefix-arg)
                                           current-prefix-arg)))
                 
                 (call-interactively #'run-dig t)))
      (defalias '*dig (if-fn% 'dig 'dig
                              #'dig
                        #'run-dig)))))


(when-fn% 'ifconfig 'net-utils
  (defun *ifconfig (&optional arg)
    "Run `ifconfig-program' and display diagnostic output."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Ifconfig options: "))))
    (require 'net-utils)
    (if-var% ifconfig-program-options 'net-utils
             (let ((ifconfig-program-options (if arg (split-string* arg " " t)
                                               ifconfig-program-options)))
               (revert-buffer-with-coding-system*
                (ifconfig)
                (concat "*" ifconfig-program  "*")))
      (let ((ipconfig-program-options (if arg (split-string* arg " " t)
                                        ipconfig-program-options)))
        (revert-buffer-with-coding-system*
         (ipconfig)
         (concat "*" ipconfig-program "*"))))))


(when-fn% 'netstat 'net-utils
  (defun *netstat (&optional arg)
    "Run `netstat-program' and display diagnostic output."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Netstat options: "))))
    (require 'net-utils)
    (let ((netstat-program-options (if arg (split-string* arg " " t)
                                     netstat-program-options)))
      (revert-buffer-with-coding-system* (netstat) "*netstat*"))))


(when-fn% 'ping 'net-utils
  (defun *ping (&optional arg)
    "Run `ping-program' for host."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Ping options: "))))
    (require 'net-utils)
    (let* ((ping-program-options (if arg (split-string* arg " " t)
                                   ping-program-options))
           (ipv6 (and (executable-find% "ping6")
                      (member** "-6" ping-program-options :test #'string=)))
           (ping-program-options (remove-if*
                                     (lambda (x)
                                       (when ipv6 (string= "-6" x)))
                                     ping-program-options))
           (ping-program (if ipv6 "ping6" ping-program)))
      (call-interactively #'ping t))))


(when-fn% 'route 'net-utils
  (defun *route (&optional arg)
    "Run `route-program' and display diagnostic output."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Route options: "))))
    (require 'net-utils)
    (let ((route-program-options (if arg (split-string* arg " " t)
                                   route-program-options)))
      (revert-buffer-with-coding-system* (route) "*route*"))))


(when-fn% 'traceroute 'net-utils
  (defun *traceroute (&optional arg)
    "Run `traceroute-program' for target."
    (interactive (when current-prefix-arg
                   (list (read-from-minibuffer "Traceroute options: "))))
    (require 'net-utils)
    (let* ((traceroute-program-options (if arg (split-string* arg " " t)
                                         traceroute-program-options))
           (ipv6 (and (executable-find% "traceroute6")
                      (member** "-6" traceroute-program-options
                                :test #'string=)))
           (traceroute-program-options (remove-if*
                                           (lambda (x)
                                             (when ipv6 (string= "-6" x)))
                                           traceroute-program-options))
           (traceroute-program (if ipv6 "traceroute6" traceroute-program)))
      (revert-buffer-with-coding-system*
       (call-interactively #'traceroute t)
       (with-no-warnings
         (require 'cl)
         (find "^Traceroute.*$" (buffer-list)
               :test (lambda (re x)
                       (string-match re (buffer-name x)))))))))

