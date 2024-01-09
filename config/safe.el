;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; safe.el
;;;;

;;; default `:safe'
(setq% enable-local-variables :safe 'files)

(defmacro safe-local-variable* (var &optional fn)
  "Safe local VAR with FN."
  `(put ,var 'safe-local-variable (or ,fn #'true)))

(defun self-safe-init! ()
  "Initialize :safe-local-variable from \\=`*self-env-spec*\\='."
  (when (*self-env-spec* :get :edit :allowed)
    (dolist* (x (*self-env-spec* :get :edit :safe-local-variable))
      (safe-local-variable* x))))

(self-safe-init!)

(provide 'safe)

;; end of safe.el
