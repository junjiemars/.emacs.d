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


(when (*self-env-spec* :get :edit :allowed)
  
  (with-eval-after-load 'sh-script
    (setq% sh-basic-offset (*self-env-spec* :get :edit :tab-width)
           'sh-script))

  ;; (with-eval-after-load 'cc-mode
  ;;   (setq% c-basic-offset (*self-env-spec* :get :edit :tab-width)
  ;;          'cc-mode))

  (let ((modes (*self-env-spec* :get
                                :edit :disable-indent-tabs-mode)))
    (when (consp modes)
      (dolist* (x (*self-env-spec* :get :edit
                                   :disable-indent-tabs-mode))
        (add-hook x #'disable-indent-tabs-mode)))))



 ;; end of on-indent-autoload.el
