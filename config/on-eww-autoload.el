;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eww-autoload.el
;;;;


(feature-eww-supported-p
  
  (defun toggle-browser! (&optional arg)
    "Toggle default browser to `eww' or not.
With prefix argument ARG, `eww' as default browser if ARG is
non-nil, otherwise not.  See also: `browser-url-browser-function'."
    (interactive "P")
    (eval-when-compile (require 'browse-url))
    (setq browse-url-browser-function
          (if (null arg)
              (if (eq browse-url-browser-function 'browse-url-default-browser)
                  #'eww-browse-url
                #'browse-url-default-browser)
            #'eww-browse-url))
    (message "eww as default browser %s"
             (if (eq browse-url-browser-function 'browse-url-default-browser)
                 "disabled"
               "enabled"))))


(feature-eww-supported-p
  (defun set-eww-mode! ()
    (toggle-truncate-lines nil)))


(feature-eww-supported-p
  (with-eval-after-load 'eww
    (add-hook 'eww-mode-hook #'set-eww-mode!)))

