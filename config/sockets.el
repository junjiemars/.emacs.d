;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sockets.el
;;;;


(defmacro-if-feature% socks)

(defmacro when-feature-socks% (&rest body)
  "When \\=`socks\\=', do BODY."
  (if-feature-socks%
      `(progn% ,@body)
    `(comment ,@body)))


(defmacro when-fn-url-open-stream% (&rest body)
  "When% \\=`url-open-stream\\='."
  `(when-feature-socks%
    (when-fn% 'url-open-stream 'url-gw
      ,@body)))

(when-fn-url-open-stream%
 (defadvice url-open-stream
     (around url-open-stream-around first compile disable)
   "Fix the \\=`url-gateway-method\\=' bug in \\=`url-http\\='."
   (if-version% <= 25
                (let ((gateway-method
                       (if (eq 'socks url-gateway-method)
                           'socks
                         gateway-method)))
                  ad-do-it)
     (let ((url-gateway-method *url-gateway-method*))
       ad-do-it))))

(when-feature-socks%
 (when-version% > 25
   (defvar *url-gateway-method* nil
     "Alias of \\=`url-gateway-method\\=' used to fix bug in
\\=`url-http\\='.")))

(when-feature-socks%
 (defvar *open-network-stream* nil
   "Alias of \\=`open-network-stream\\=' used to fix bug in
 \\=`url-http\\='."))

(when-fn-url-open-stream%
 (defun ad*-activate-url-open-stream (&optional activate)
   (if activate
       (progn
         (ad-enable-advice #'url-open-stream 'around
                           "url-open-stream-around")
         (ad-activate #'url-open-stream t))
     (ad-deactivate #'url-open-stream)
     (ad-disable-advice #'url-open-stream 'around
                        "url-open-stream-around")
     (ad-clear-cache #'url-open-stream))))

(when-feature-socks%
 (defun toggle-socks! (&optional arg)
   "Toggle \\=`url-gatewary-method\\=' to socks or native.\n
With prefix argument ARG, \\=`url-gatewary-method\\=' via socks
if ARG is greater than 1, otherwise via native."
   (interactive "p")
   (let ((native (lambda ()
                   (setq% url-gateway-method 'native 'url-vars)
                   (setq% socks-server
                          (list "Default server"
                                nil
                                (*self-env-spec* :get :socks :port)
                                (*self-env-spec* :get :socks :version))
                          'socks)
                   (when *open-network-stream*
                     (setf (symbol-function 'open-network-stream)
                           *open-network-stream*))))
         (socks (lambda ()
                  (setq% url-gateway-method 'socks 'url-vars)
                  (setq% socks-server
                         (list "Default server"
                               (*self-env-spec* :get :socks :server)
                               (*self-env-spec* :get :socks :port)
                               (*self-env-spec* :get :socks :version))
                         'socks)
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
       (when-fn-url-open-stream%
        (ad*-activate-url-open-stream activate))
       (message "socks%s as url gateway %s"
                (list (*self-env-spec* :get :socks :server)
                      (*self-env-spec* :get :socks :port)
                      (*self-env-spec* :get :socks :version))
                (if activate "enabled" "disabled"))))))


(defun self-socks-init! ()
  "Initialize :socks spec from \\=`*self-env-spec*\\='."
  (when-feature-socks%
   (when (*self-env-spec* :get :socks :allowed)
     (toggle-socks!))))



(provide 'sockets)

;; end of sockets.el file
