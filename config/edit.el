;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; edit.el
;;;;
;; Commentary: essential editing environment.
;;;;


(defun edit-spec->* (&optional key)
  "Extract :edit from env-spec via KEY."
  (cond (key (*self-env-spec* :get :edit key))
        (t (*self-env-spec* :get :edit))))



(defun self-edit-env->disable-indent-tabs-mode ()
  "Disable \\=`indent-tabs-mode\\=' in major mode."
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun self-edit-env->delete-trailing-whitespace ()
  "\\=`delete-trailing-whitespace\\=' before save."
  (when (apply #'derived-mode-p
               (edit-spec->* :delete-trailing-whitespace))
    (delete-trailing-whitespace)))

(defun self-edit-init! ()
  "Initialize edit spec from \\=`*self-edit-spec*\\='"
  (when (edit-spec->* :allowed)
    ;; indent
    (dolist (x (edit-spec->* :indent))
      (set (car x) (or (cdr x) (edit-spec->* :tab-width))))

    ;; disable `indent-tabs-mode'
    (dolist (m (edit-spec->* :disable-indent-tabs-mode))
      (let ((h (intern (format "%s-hook" m))))
        (add-hook h #'self-edit-env->disable-indent-tabs-mode)))

    ;; default `tab-width' and `standard-indent'
    (setq-default tab-width (edit-spec->* :tab-width)
                  standard-indent tab-width)

    ;; default `auto-save-default'
    (setq auto-save-default (edit-spec->* :auto-save-default))

    ;; enable `narrow-to-region'
    (put 'narrow-to-region 'disabled
         (null (edit-spec->* :narrow-to-region)))
    ;; enable `narrow-to-page'
    (when% (get 'narrow-to-page 'disabled)
      (put 'narrow-to-page 'disabled nil))

    ;; `delete-trailing-whitespace' before save
    (append! #'self-edit-env->delete-trailing-whitespace before-save-hook)))

;; end of self-edit

(provide 'edit)

;; end of edit.el
