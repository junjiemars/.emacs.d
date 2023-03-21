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


(with-eval-after-load 'view
  ;; keep `view-mode' when quit
  (define-key% view-mode-map (kbd "q") #'quit-window))







;; end of on-help-autoload.el
