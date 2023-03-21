;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-help-autoload.el
;;;;



(with-eval-after-load 'help-mode

  ;; open emacs source in `view-mode'
  (lexical-let% ((help (button-type-get 'help-function-def 'help-function)))
    (button-type-put
     'help-function-def 'help-function
     (lambda (fn &optional file)
       (funcall help fn file)
       (view-mode 1))))

  ;; open emacs source in `view-mode'
  (lexical-let% ((help (button-type-get 'help-variable-def 'help-function)))
    (button-type-put
     'help-variable-def 'help-function
     (lambda (var &optional file)
       (funcall help var file)
       (view-mode 1)))))


(when-version% > 24.0
  ;; `View-quit' has different behaviors between Emacs24.0- and Emacs24.0+
  (defadvice View-quit (after view-quit-after disable)
    (quit-window))

  (with-eval-after-load 'view
		(ad-enable-advice #'View-quit 'after "view-quit-after")
    (ad-activate #'View-quit t)))


;; treat `read-only-mode' as `view-mode'
(setq view-read-only t)



;; end of on-help-autoload.el
