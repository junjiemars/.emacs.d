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


;;; `safe-local-variable'
(when (*self-env-spec* :get :edit :allowed)
  (dolist* (x (*self-env-spec* :get :edit :safe-local-variable))
    (safe-local-variable* x)))


;;; enable `narrow-to-page'
(when% (get 'narrow-to-page 'disabled)
  (put 'narrow-to-page 'disabled nil))


(provide 'safe)

;; end of safe.el
