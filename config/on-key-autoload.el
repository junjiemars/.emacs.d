;;; on-edit-autoload.el --- editing -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-key-autoload.el
;;;;
;;; Commentary:
;;


(let ((key (*self-env-spec* :get :key)))
  (when (self-spec-> key :allowed)
    (let ((modifier (self-spec-> key :modifier)))
      (dolist* (x modifier)
        (set (car x) (cdr x))))))



 ; end of on-key-autoload.el
