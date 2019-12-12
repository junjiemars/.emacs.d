;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sockets.el
;;;;


(defmacro-if-feature% socks)

(comment
 (and (require 'url-vars nil t)
      (require 'socks nil t)
      (boundp 'url-gateway-method)
      (boundp 'socks-server)))

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
                          'socks)))
         (socks (lambda ()
                  (setq% url-gateway-method 'socks 'url-vars)
                  (setq% socks-server
                         (list "Default server"
                               (self-spec->*env-spec :socks :server)
                               (self-spec->*env-spec :socks :port)
                               (self-spec->*env-spec :socks :version))
                         'socks))))
     ;; (require 'url)
     (if (null arg)
         (if (eq url-gateway-method 'native) (funcall socks) (funcall native))
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


;; end of file
