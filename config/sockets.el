;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sockets.el
;;;;


(defun socks-spec->* (&optional key)
  "Extract :socks from env-spec via KEY."
  (cond (key (*self-env-spec* :get :socks key))
        (t (*self-env-spec* :get :socks))))



(defmacro when-url-open-stream% (&rest body)
  "When% \\=`url-open-stream\\='."
  `(when-feature% socks
     (when-fn% url-open-stream url-gw
       ,@body)))

(when-url-open-stream%
 (defun url-open-stream* (name buffer host service &optional gateway-method)
   (if-version%
       <= 25
       (let ((gateway-method (if (eq 'socks url-gateway-method)
                                 'socks
                               gateway-method)))
         (funcall (symbol-function '_url-open-stream_)
                  name buffer host service gateway-method))
     (let ((url-gateway-method *url-gateway-method*))
       (funcall (symbol-function '_url-open-stream_)
                name buffer host service gateway-method)))))

(when-feature% socks
  (when-version% > 25
    (defvar *url-gateway-method* nil
      "Alias of \\=`url-gateway-method\\=' used to fix bug in
\\=`url-http\\='.")))

(when-feature% socks
  (defvar *open-network-stream* nil
    "Alias of \\=`open-network-stream\\=' used to fix bug in
 \\=`url-http\\='."))

(when-feature% socks
  (defun toggle-socks! (&optional arg)
    "Toggle \\=`url-gatewary-method\\=' to socks or native.\n
With prefix argument ARG, \\=`url-gatewary-method\\=' via socks
if ARG is greater than 1, otherwise via native."
    (interactive "p")
    (let ((native (lambda ()
                    (setq% url-gateway-method 'native url-vars)
                    (setq% socks-server
                           (list "Default server"
                                 nil
                                 (socks-spec->* :port)
                                 (socks-spec->* :version))
                           socks)
                    (when *open-network-stream*
                      (setf (symbol-function 'open-network-stream)
                            *open-network-stream*))))
          (socks (lambda ()
                   (setq% url-gateway-method 'socks url-vars)
                   (setq% socks-server
                          (list "Default server"
                                (socks-spec->* :server)
                                (socks-spec->* :port)
                                (socks-spec->* :version))
                          socks)
                   (setf (symbol-function 'open-network-stream)
                         #'socks-open-network-stream)))
          (ver (if (null arg) 4 arg)))
      ;; (require 'url)
      (require 'socks)
      (unless *open-network-stream*
        (setf (symbol-value '*open-network-stream*)
              (symbol-function 'open-network-stream)))
      (if (= 1 ver)
          (funcall (if (eq url-gateway-method 'native) socks native))
        (funcall (if (> ver 1) socks native)))
      (let ((activate (eq 'socks url-gateway-method)))
        (when-version% > 25
          (setq *url-gateway-method* url-gateway-method))
        (when-url-open-stream%
         (defadvice* '_url-open-stream_ 'url-open-stream #'url-open-stream*))
        (message "socks%s as url gateway %s"
                 (list (socks-spec->* :server)
                       (socks-spec->* :port)
                       (socks-spec->* :version))
                 (if activate "enabled" "disabled"))))))


(defun self-socks-init! ()
  "Initialize :socks spec from \\=`*self-env-spec*\\='."
  (when-feature% socks
    (when (socks-spec->* :allowed)
      (toggle-socks!))))



(provide 'sockets)

;; end of sockets.el file
