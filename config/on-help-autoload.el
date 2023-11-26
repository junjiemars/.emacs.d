;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-help-autoload.el
;;;;


(defun on-help-mode-init! ()
  "On \\=`help-mode\\=' initialization."
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


;;; `help-mode' after load
(eval-after-load 'help-mode #'on-help-mode-init!)


;; end of on-help-autoload.el
