;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; helps.el
;;;;

(defun on-help-init! ()
  "On \\=`help\\=' initialization."
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



(provide 'helps)

;; end of helps.el
