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

;;;
;; Clean Emacs' user files
;;;

(defun clean-versioned-dirs (dirs &optional scope)
  "Clean versioned SCOPEd DIRS."
  (dolist (d dirs)
    (setq d (concat (emacs-home* d) "/"))
    (when (and d (file-exists-p d))
      (dolist (f (directory-files d nil "^[gt]_[0-9]+[.0-9]*"))
        (when (cond ((and scope (eq :8 scope)) t)
                    ((and scope (eq :< scope))
                     (let ((v (string-match*
                               (format "^%s\\([.0-9]+\\)$" +v-prefix+)
                               f 1)))
                       (and v (< (string-to-number v) +emacs-version+))))
                    (t (string-equal (v-name) f)))
          (shell-command* (if-platform% windows-nt
                              (concat "rmdir /Q /S ")
                            (concat "rm -r "))
            (concat d f)))))))

(defun reset-emacs (&optional do? ver?)
  "Clean all compiled files and dot files, then kill Emacs."
  (interactive)
  (when (if-interactive%
            (or do? (yes-or-no-p "Reset emacs?"))
          do?)
    (let ((ds (let ((xs nil)
                    (d1 (directory-files (emacs-home%) nil "^\\.[a-z]+")))
                (dolist (d d1 xs)
                  (unless (member d '(".git" ".gitignore" ".github"))
                    (setq xs (cons d xs))))))
          (ns `("private" "config" "theme")))
      (clean-versioned-dirs (nconc ds ns) ver?))
    (setq kill-emacs-hook nil)
    (setq% kill-emacs-query-functions nil)
    (kill-emacs 0)))

;; end of Clean Emacs' user files

(provide 'edit)

;; end of edit.el
