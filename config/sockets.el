;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sockets.el
;;;;


(defmacro-if-feature% socks)


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
                      (when-version% <= 25
                        (ad-activate #'url-http t))))
            (socks (lambda ()
                     (setq% url-gateway-method 'socks 'url-vars)
                     (setq% socks-server
                            (list "Default server"
                                  (self-spec->*env-spec :socks :server)
                                  (self-spec->*env-spec :socks :port)
                                  (self-spec->*env-spec :socks :version))
                            'socks)
                     (when-version% <= 25
                       (ad-deactivate #'url-http)))))
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


(when-version% <= 25

  (when-fn% 'url-http 'url-http

    (defadvice url-http (before url-http-before compile)
      "Fix the `url-gateway-method' bug in `url-http'."
      (ad-set-arg 4 (let ((gateway-method (ad-get-arg 4)))
                      (cond ((null gateway-method) url-gateway-method)
                            ((and (eq 'socks url-gateway-method)
                                  (not (eq gateway-method url-gateway-method)))
                             url-gateway-method)
                            (t gateway-method)))))))


(if-feature-socks%
    (when (self-spec->*env-spec :socks :allowed)

      (toggle-socks! t)))


;; end of sockets.el file
