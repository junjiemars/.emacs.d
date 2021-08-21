;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; safe.el
;;;;


(defun toggle-enable-local-variables (&optional what)
  "Toggle `local-enable-local-variables' to WHAT."
  (interactive
   (list (when current-prefix-arg
           (read-string (format "Choose (%s) "
                                (mapconcat #'symbol-name
                                           '(t :safe :all)
                                           "|"))))))
  (setq local-enable-local-variables (and (stringp what)
                                          (intern what))))


;;; It's buggy.
;; (when (*self-env-spec* :get :edit :allowed)
;;   ;; default `local-enable-local-variables'
;;   (setq local-enable-local-variables
;;         (*self-env-spec* :get :edit :local-enable-local-variables)))



(provide 'safe)

;; end of safe.el
