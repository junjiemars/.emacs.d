;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; safe.el
;;;;

;;; default `:safe'
(setq% enable-local-variables :safe 'files)


(defmacro safe-local-variable* (var &optional fn)
  "Safe local VAR with FN, see \\=`enable-local-variables\\='"
  `(put ,var 'safe-local-variable (or ,fn #'true)))


(defun self-safe-init! ()
  "Initialize edit spec from \\=`*self-env-spec*\\='."
  (let ((spec (*self-env-spec* :get :edit)))
    (when (self-spec-> spec :allowed)
      (dolist* (x (self-spec-> spec :safe-local-variable))
        (safe-local-variable* x)))))

(self-safe-init!)

(provide 'safe)

;; end of safe.el
