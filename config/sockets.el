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

          (defadvice url-http (around url-http-around compile)
            "Fix the `url-gateway-method' bug in `url-http'."
            (if (eq 'socks url-gateway-method)
                (let ((gateway-method url-gateway-method))
                  ad-do-it)
              ad-do-it))

        (defadvice url-http (around url-http-around compile)
          "Fix the `url-gateway-method' bug in `url-https'."
          (let ((url-gateway-method (if (eq *url-gateway-method* 'socks)
                                        'socks
                                      'tls)))
            ad-do-it)))))

(if-feature-socks%
    (when-version% > 25

      (defvar *url-gateway-method* nil
        "Alias of `url-gateway-method' used to fix the bug in `url-http'.")))

(if-feature-socks%

    (defmacro url-http-ad-activate (activate)
      "Activate or deactive `url-http'."
      `(progn
         (when-version% > 25
           (setq *url-gateway-method* url-gateway-method))
         (if ,activate
             (ad-activate #'url-http t)
           (ad-deactivate #'url-http)))))


(if-feature-socks%

    (defun toggle-socks! (&optional arg)
      "Toggle `url-gatewary-method' to socks or native.
With prefix argument ARG, `url-gatewary-method' via socks if ARG is 
positive, otherwise via native."
      (interactive "P")
      (let ((native (lambda ()
                      (setq% url-gateway-method 'native 'url-vars)
                      (setq% socks-server
                             (list "Default server"
                                   nil
                                   (self-spec->*env-spec :socks :port)
                                   (self-spec->*env-spec :socks :version))
                             'socks)
                      (url-http-ad-activate t)))
            (socks (lambda ()
                     (setq% url-gateway-method 'socks 'url-vars)
                     (setq% socks-server
                            (list "Default server"
                                  (self-spec->*env-spec :socks :server)
                                  (self-spec->*env-spec :socks :port)
                                  (self-spec->*env-spec :socks :version))
                            'socks)
                     (url-http-ad-activate nil))))
        ;; (require 'url)
        (if (null arg)
            (if (eq url-gateway-method 'native)
                (funcall socks)
              (funcall native))
          (funcall socks))
        (message "socks%s as url gateway %s"
                 (list (self-spec->*env-spec :socks :server)
                       (self-spec->*env-spec :socks :port)
                       (self-spec->*env-spec :socks :version))
                 (if (eq url-gateway-method 'native)
                     "disabled"
                   "enabled")))))






(if-feature-socks%
    (when (self-spec->*env-spec :socks :allowed)

      (toggle-socks! t)))


;; end of sockets.el file
