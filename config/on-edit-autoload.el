;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-edit-autoload.el
;;;;


(defun self-edit-env->disable-indent-tabs-mode ()
  "Disable \\=`indent-tabs-mode\\=' in major mode."
  (set (make-local-variable 'indent-tabs-mode) nil))


(defun self-edit-env->delete-trailing-whitespace ()
  "\\=`delete-trailing-whitespace\\=' before save."
  (when (apply #'derived-mode-p
               (*self-env-spec* :get :edit :delete-trailing-whitespace))
    (delete-trailing-whitespace)))


(defun self-edit-init! ()
  "Initialize edit spec from \\=`*self-env-spec*\\='"
  (let ((edit (*self-env-spec* :get :edit)))
		(when (self-spec-> edit :allowed)
			;; indent
			(let ((indent (self-spec-> edit :indent))
						(width (self-spec-> edit :tab-width)))
				(dolist* (x indent)
					(set (car x) (or (cdr x) width))))

			;; disable `indent-tabs-mode'
			(let ((modes (self-spec-> edit :disable-indent-tabs-mode)))
				(dolist* (m modes)
          (let ((h (intern-soft (format "%s-hook" m))))
            (when h
              (add-hook
               h #'self-edit-env->disable-indent-tabs-mode)))))

      ;; default `tab-width' and `standard-indent'
      (let ((w (self-spec-> edit :tab-width)))
        (setq-default tab-width w standard-indent w))

      ;; default `auto-save-default'
      (setq auto-save-default
            (self-spec-> edit :auto-save-default))

      ;; enable `narrow-to-region'
      (put 'narrow-to-region 'disabled
           (null (self-spec-> edit :narrow-to-region)))
      ;; enable `narrow-to-page'
      (when% (get 'narrow-to-page 'disabled)
        (put 'narrow-to-page 'disabled nil))

      ;; `delete-trailing-whitespace' before save
      (append! #'self-edit-env->delete-trailing-whitespace
               before-save-hook))))




(self-edit-init!)


;; end of on-edit-autoload.el
