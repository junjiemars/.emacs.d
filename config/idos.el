;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; idos.el
;;;;

(defun on-ido-init! ()
  "On \\=`ido\\=' intialization."
  (when-version% > 28
    (make-thread*
     (lambda () (require 'dired-x nil t))))
  (define-key% (current-global-map) "5r"
               #'ido-find-file-read-only-other-frame)
  (define-key% (current-global-map) "4r"
               #'ido-find-file-read-only-other-window)
  (define-key% (current-global-map) ""
               #'ido-find-file-read-only)
  (setq% ido-enable-flex-matching t 'ido))



(provide 'idos)

;; end of idos.el
