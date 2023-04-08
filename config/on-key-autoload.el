;;; on-edit-autoload.el --- editing -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-key-autoload.el
;;;;
;;; Commentary:
;;


(when (*self-env-spec* :get :key :allowed)

  (let ((modifier (*self-env-spec* :get :key :modifier)))
    (dolist* (x modifier)
      (set (car x) (cdr x)))))



 ; end of on-key-autoload.el
