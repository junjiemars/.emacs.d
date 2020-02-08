;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sockets.el
;;;;


(defmacro-if-feature% socks)


(if-feature-socks%
    (when-fn% 'url-http 'url-http
      (if-version%
          <= 25

          (defadvice url-http (around url-http-around activate disable)
            "Fix the `url-gateway-method' bug in `url-http'."
            (if (eq 'socks url-gateway-method)
                (let ((gateway-method url-gateway-method))
                  ad-do-it)
              ad-do-it))

        (defadvice url-http (around url-http-around disable)
          "Fix the `url-gateway-method' bug in `url-https'."
          (let ((url-gateway-method (if (eq *url-gateway-method* 'socks)
                                        'socks
                                      'tls)))
            ad-do-it)))))

(if-feature-socks%
    (when-version% > 25

      (defvar *url-gateway-method* nil
        "Alias of `url-gateway-method' used to fix bug in `url-http'.")))

(if-feature-socks%

    (defvar *open-network-stream* nil
      "Alias of `open-network-stream' used to fix bug in `url-http'."))



(if-feature-socks%

    (defun toggle-socks! (&optional arg)
      "Toggle `url-gatewary-method' to socks or native.
With prefix argument ARG, `url-gatewary-method' via socks if ARG is 
greate than 1, otherwise via native."
      (interactive "p")
      (let ((native (lambda ()
                      (setq% url-gateway-method 'native 'url-vars)
                      (setq% socks-server
                             (list "Default server"
                                   nil
                                   (self-spec->*env-spec :socks :port)
                                   (self-spec->*env-spec :socks :version))
                             'socks)
                      (when *open-network-stream*
                        (setf (symbol-function 'open-network-stream)
                              *open-network-stream*))
                      ))
            (socks (lambda ()
                     (setq% url-gateway-method 'socks 'url-vars)
                     (setq% socks-server
                            (list "Default server"
                                  (self-spec->*env-spec :socks :server)
                                  (self-spec->*env-spec :socks :port)
                                  (self-spec->*env-spec :socks :version))
                            'socks)
                     (setf (symbol-function 'open-network-stream)
                           #'socks-open-network-stream))))
        ;; (require 'url)
        (require 'socks)
        (unless *open-network-stream*
          (setf (symbol-value '*open-network-stream*)
                (symbol-function 'open-network-stream)))
        (if (or (null arg) (= 1 arg))
            (funcall (if (eq url-gateway-method 'native) socks native))
          (funcall (if (> arg 1) socks native)))
        (let ((activated (eq 'socks url-gateway-method)))
          (when-version% > 25
            (setq *url-gateway-method* url-gateway-method))
          (if activated
              (progn
                (ad-enable-advice #'url-http 'around "url-http-around")
                (ad-activate #'url-http t))
            (ad-deactivate #'url-http)
            (ad-disable-advice #'url-http 'around "url-http-around"))
          (message "socks%s as url gateway %s"
                   (list (self-spec->*env-spec :socks :server)
                         (self-spec->*env-spec :socks :port)
                         (self-spec->*env-spec :socks :version))
                   (if activated "enabled" "disabled")))))
  )



(if-feature-socks%

    (when (self-spec->*env-spec :socks :allowed)
      (toggle-socks! 4)))


;; end of sockets.el file
