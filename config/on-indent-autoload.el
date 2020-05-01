;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-indent-autoload.el
;;;;


(defun disable-indent-tabs-mode ()
  "Disable `indent-tabs-mode' in major mode."
  (set (make-local-variable 'indent-tabs-mode) nil))


(when (self-spec->*env-spec :edit :allowed)
  
  (with-eval-after-load 'sh-script
    (setq% sh-basic-offset (self-spec->*env-spec :edit :tab-width)
           'sh-script))

  ;; (with-eval-after-load 'cc-mode
  ;;   (setq% c-basic-offset (self-spec->*env-spec :edit :tab-width)
  ;;          'cc-mode))

  (let ((modes (self-spec->*env-spec
                 :edit :disable-indent-tabs-mode)))
    (when (consp modes)
      (mapc (lambda (x)
              (add-hook x #'disable-indent-tabs-mode))
            (self-spec->*env-spec :edit
                                  :disable-indent-tabs-mode)))))



 ;; end of on-indent-autoload.el
