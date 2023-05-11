;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-indent-autoload.el
;;;;


(defun edit-env->disable-indent-tabs-mode ()
  "Disable `indent-tabs-mode' in major mode."
  (set (make-local-variable 'indent-tabs-mode) nil))


(when (*self-env-spec* :get :edit :allowed)
  (make-thread*
   (lambda ()
     ;; indent
     (let ((indent (*self-env-spec* :get :edit :indent))
           (width (*self-env-spec* :get :edit :tab-width)))
       (dolist* (x indent)
         (set (car x) (or (cdr x) width))))
     ;; disable `indent-tabs-mode'
     (let ((modes (*self-env-spec* :get :edit :disable-indent-tabs-mode)))
       (when (consp modes)
         (dolist* (x modes)
           (add-hook x #'edit-env->disable-indent-tabs-mode)))))))


 ;; end of on-indent-autoload.el
